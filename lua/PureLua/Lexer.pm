package PureLua::Lexer;

use strict;
use warnings;

# Lua 5.1 lexer, scoped to what the Midea device-protocol scripts actually
# use (see .notes/2026-11-16-pure-perl-lua-engine-design.md): no goto
# labels (5.2+), no hex floats (5.2+), no \xXX escapes (5.2+).

my %KEYWORDS = map { $_ => 1 } qw(
    and break do else elseif end false for function if in
    local nil not or repeat return then true until while
);

# Longest-match-first; required for '...' vs '..' vs '.', etc.
my @SYMBOLS = (
    '...', '..', '==', '~=', '<=', '>=',
    '+', '-', '*', '/', '%', '^', '#', '<', '>', '=',
    '(', ')', '{', '}', '[', ']', ';', ':', ',', '.',
);

sub new {
    my ($class, $src) = @_;
    return bless { src => $src, pos => 0, line => 1 }, $class;
}

sub tokenize {
    my ($class, $src) = @_;
    my $self = $class->new($src);
    my @tokens;
    while (1) {
        my $tok = $self->_next_token;
        push @tokens, $tok;
        last if $tok->{type} eq 'eof';
    }
    return \@tokens;
}

sub _error {
    my ($self, $msg) = @_;
    die "PureLua::Lexer: $msg at line $self->{line}\n";
}

sub _peek_char {
    my ($self, $offset) = @_;
    $offset ||= 0;
    return substr($self->{src}, $self->{pos} + $offset, 1);
}

sub _next_token {
    my ($self) = @_;
    my $src = $self->{src};

    # A '#!' shebang line is only special at the very start of the file.
    if ($self->{pos} == 0 && substr($src, 0, 1) eq '#') {
        $self->_skip_line_comment;
    }

    $self->_skip_whitespace_and_comments;

    if ($self->{pos} >= length($src)) {
        return { type => 'eof', value => undef, line => $self->{line} };
    }

    my $line = $self->{line};
    my $c    = $self->_peek_char;

    if ($c eq '[') {
        my $level = $self->_try_long_bracket_open;
        if (defined $level) {
            my $str = $self->_read_long_bracket_body($level, $line);
            return { type => 'string', value => $str, line => $line };
        }
    }

    if ($c eq '"' || $c eq "'") {
        $self->{pos}++;
        my $str = $self->_read_short_string($c, $line);
        return { type => 'string', value => $str, line => $line };
    }

    if ($c =~ /[0-9]/ || ($c eq '.' && $self->_peek_char(1) =~ /[0-9]/)) {
        return { type => 'number', value => $self->_read_number, line => $line };
    }

    if ($c =~ /[A-Za-z_]/) {
        pos($src) = $self->{pos};
        $src =~ /\G([A-Za-z_][A-Za-z0-9_]*)/g or die; # cannot fail, $c already matched
        my $name = $1;
        $self->{pos} = pos($src);
        if ($KEYWORDS{$name}) {
            return { type => 'keyword', value => $name, line => $line };
        }
        return { type => 'name', value => $name, line => $line };
    }

    for my $sym (@SYMBOLS) {
        my $len = length $sym;
        if (substr($src, $self->{pos}, $len) eq $sym) {
            $self->{pos} += $len;
            return { type => $sym, value => $sym, line => $line };
        }
    }

    $self->_error("unexpected character '$c'");
}

sub _skip_whitespace_and_comments {
    my ($self) = @_;
    my $src = $self->{src};
    while ($self->{pos} < length($src)) {
        my $c = substr($src, $self->{pos}, 1);
        if ($c eq ' ' || $c eq "\t" || $c eq "\f" || $c eq "\x0b") {
            $self->{pos}++;
        }
        elsif ($c eq "\n" || $c eq "\r") {
            $self->_skip_newline;
        }
        elsif ($c eq '-' && substr($src, $self->{pos} + 1, 1) eq '-') {
            $self->{pos} += 2;
            my $level = $self->_try_long_bracket_open;
            if (defined $level) {
                $self->_read_long_bracket_body($level, $self->{line});
            }
            else {
                $self->_skip_line_comment;
            }
        }
        else {
            last;
        }
    }
}

# Treats \r\n and \n\r as a single newline, like real Lua.
sub _skip_newline {
    my ($self) = @_;
    my $src = $self->{src};
    my $first = substr($src, $self->{pos}, 1);
    $self->{pos}++;
    my $second = substr($src, $self->{pos}, 1);
    if (($second eq "\n" || $second eq "\r") && $second ne $first) {
        $self->{pos}++;
    }
    $self->{line}++;
}

sub _skip_line_comment {
    my ($self) = @_;
    my $src = $self->{src};
    while ($self->{pos} < length($src)) {
        my $c = substr($src, $self->{pos}, 1);
        last if $c eq "\n" || $c eq "\r";
        $self->{pos}++;
    }
}

# At a '[', tries to match a long-bracket opener '[' '='* '['. On success
# consumes it and returns the '=' count; on failure consumes nothing.
sub _try_long_bracket_open {
    my ($self) = @_;
    my $src = $self->{src};
    pos($src) = $self->{pos};
    if ($src =~ /\G\[(=*)\[/g) {
        $self->{pos} = pos($src);
        return length($1);
    }
    return undef;
}

sub _read_long_bracket_body {
    my ($self, $level, $open_line) = @_;
    my $src   = $self->{src};
    my $start = $self->{pos};
    my $close = ']' . ('=' x $level) . ']';
    my $idx   = index($src, $close, $start);
    $self->_error("unterminated long bracket opened at line $open_line") if $idx < 0;

    my $content = substr($src, $start, $idx - $start);
    my $nl_count = () = $content =~ /\n/g;
    $self->{line} += $nl_count;
    $self->{pos} = $idx + length($close);

    # A newline right after the opening bracket is dropped.
    $content =~ s/\A\r\n|\A\n\r|\A\n|\A\r//;
    return $content;
}

sub _read_short_string {
    my ($self, $quote, $start_line) = @_;
    my $src = $self->{src};
    my @chars;
    while (1) {
        $self->_error("unterminated string opened at line $start_line")
            if $self->{pos} >= length($src);
        my $c = substr($src, $self->{pos}, 1);

        if ($c eq $quote) {
            $self->{pos}++;
            last;
        }
        if ($c eq "\n" || $c eq "\r") {
            $self->_error("unterminated string opened at line $start_line (newline in short string)");
        }
        if ($c eq "\\") {
            $self->{pos}++;
            push @chars, $self->_read_escape($start_line);
            next;
        }
        push @chars, $c;
        $self->{pos}++;
    }
    return join('', @chars);
}

my %SIMPLE_ESCAPES = (
    a => "\a", b => "\b", f => "\f", n => "\n",
    r => "\r", t => "\t", v => "\x0b",
    '\\' => "\\", '"' => '"', "'" => "'",
);

sub _read_escape {
    my ($self, $start_line) = @_;
    my $src = $self->{src};
    my $e   = substr($src, $self->{pos}, 1);

    if (exists $SIMPLE_ESCAPES{$e}) {
        $self->{pos}++;
        return $SIMPLE_ESCAPES{$e};
    }
    if ($e eq "\n" || $e eq "\r") {
        $self->_skip_newline;
        return "\n";
    }
    if ($e =~ /[0-9]/) {
        my $digits = '';
        for (1 .. 3) {
            my $d = substr($src, $self->{pos}, 1);
            last unless $d =~ /[0-9]/;
            $digits .= $d;
            $self->{pos}++;
        }
        my $code = int($digits);
        $self->_error("decimal escape '\\$digits' too large") if $code > 255;
        return chr($code);
    }
    $self->_error("invalid escape sequence '\\$e' (string opened at line $start_line)");
}

sub _read_number {
    my ($self) = @_;
    my $src = $self->{src};
    pos($src) = $self->{pos};

    if ($src =~ /\G0[xX]([0-9a-fA-F]+)/gc) {
        $self->{pos} = pos($src);
        return hex($1);
    }

    if ($src =~ m{\G(?:\d+\.\d*(?:[eE][+-]?\d+)?|\.\d+(?:[eE][+-]?\d+)?|\d+(?:[eE][+-]?\d+)?)}g) {
        my $text = $&;
        $self->{pos} = pos($src);
        return $text + 0;
    }

    $self->_error("malformed number");
}

1;
