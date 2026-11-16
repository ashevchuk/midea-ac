#!/usr/bin/env perl
use strict;
use warnings;

package PureLua::Table;

use strict;
use warnings;

sub new {
    return bless { map => {}, order => [] }, shift;
}

# Wraps a plain Perl string as a Lua string value (PureLua::Eval's
# _mkstr delegates here - this is the one place that owns the
# 'PureLua::Str' class tag). Native Perl code that populates a table
# Lua code will also index - stdlib library tables, the cjson module,
# globals by variable name - must wrap its literal keys with this, or
# they land under a different key than Lua's own string indexing produces.
sub wrap_str {
    my ($s) = @_;
    return bless \$s, 'PureLua::Str';
}

# Canonical key string. Lua strings are blessed PureLua::Str refs and
# Lua numbers are plain Perl scalars (see PureLua::Eval), so a numeric-
# looking string key ("5") and the real number key (5) land in distinct
# slots here, matching real Lua - they used to collapse together under
# an earlier, heuristic-only type distinction; see
# .notes/2026-11-16-pure-lua-engine-progress.md for why that broke.
sub _normalize_key {
    my ($key) = @_;
    die "table index is nil\n" unless defined $key;
    return "s:$$key" if ref($key) eq 'PureLua::Str';
    return "r:$key"  if ref $key;
    my $n = $key + 0;
    $n = int($n) if $n == int($n);
    return "n:$n";
}

sub get {
    my ($self, $key) = @_;
    return undef unless defined $key;
    my $entry = $self->{map}{ _normalize_key($key) };
    return defined($entry) ? $entry->[1] : undef;
}

sub set {
    my ($self, $key, $value) = @_;
    my $ck = _normalize_key($key);
    if (!defined $value) {
        if (exists $self->{map}{$ck}) {
            delete $self->{map}{$ck};
            @{ $self->{order} } = grep { $_ ne $ck } @{ $self->{order} };
        }
        return;
    }
    push @{ $self->{order} }, $ck unless exists $self->{map}{$ck};
    $self->{map}{$ck} = [ $key, $value ];
}

# [key, value] pairs in insertion order (real Lua's pairs() order is
# unspecified; insertion order is a convenient, deterministic choice).
sub pairs_list {
    my ($self) = @_;
    return [ map { $self->{map}{$_} } @{ $self->{order} } ];
}

# Lua's `#t`: a border (n where t[n]~=nil and t[n+1]==nil), found by a
# linear scan from 1. Correct for the sequential-from-1 arrays these
# scripts build; NOT verified against real Lua for sparse or 0-based
# tables - see design doc gotcha #1 (needs an empirical luajit check
# against a `bodyBytes`-shaped table before trusting this further).
sub lua_length {
    my ($self) = @_;
    return 0 unless defined $self->get(1);
    my $n = 1;
    $n++ while defined $self->get($n + 1);
    return $n;
}

1;

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

package PureLua::Pattern;

use strict;
use warnings;

# A Lua pattern matcher scoped to exactly the vocabulary the Midea
# device-protocol corpus actually uses (see
# .notes/2026-11-16-pure-perl-lua-engine-design.md): literal substrings,
# the %d escape class, character sets [...]/[^...] (with %d and a-z
# style ranges inside), and +/* quantifiers. No anchors (^/$), no
# captures, no %a/%s/%w/etc. classes, no %b/%f, no lazy '-' quantifier.
#
# Implemented by translating the restricted pattern into an equivalent
# Perl regex (the two languages agree closely enough for this subset)
# rather than writing a bespoke matcher. Anything outside the supported
# vocabulary is an explicit die, never a silent mismatch - a pattern that
# doesn't do what it looks like is a worse outcome than a loud error.

my %COMPILE_CACHE;

sub _compile {
    my ($lua_pattern) = @_;
    return $COMPILE_CACHE{$lua_pattern} ||= do {
        my $re_src = _translate($lua_pattern);
        qr/$re_src/;
    };
}

sub _translate {
    my ($pattern) = @_;
    my $out = '';
    my $i   = 0;
    my $len = length $pattern;

    while ($i < $len) {
        my $c = substr($pattern, $i, 1);

        if ($c eq '%') {
            my $n = substr($pattern, $i + 1, 1);
            die "PureLua::Pattern: pattern ends with a dangling '%'\n" if $n eq '';
            if ($n eq 'd') {
                $out .= '\d';
            }
            elsif ($n =~ /[A-Za-z]/) {
                die "PureLua::Pattern: unsupported pattern class '%$n' (only %d is implemented)\n";
            }
            else {
                $out .= quotemeta($n);
            }
            $i += 2;
            next;
        }

        if ($c eq '[') {
            my ($set, $consumed) = _translate_set($pattern, $i);
            $out .= $set;
            $i += $consumed;
            next;
        }

        if ($c eq '+' || $c eq '*') {
            $out .= $c;
            $i++;
            next;
        }

        if ($c eq '.' || $c eq '^' || $c eq '$' || $c eq '(' || $c eq ')' || $c eq '-') {
            die "PureLua::Pattern: unsupported pattern construct '$c' (out of documented scope)\n";
        }

        $out .= quotemeta($c);
        $i++;
    }
    return $out;
}

sub _translate_set {
    my ($pattern, $start) = @_;
    my $len = length $pattern;
    my $i = $start + 1; # skip '['
    my $negate = '';
    if (substr($pattern, $i, 1) eq '^') { $negate = '^'; $i++; }

    my $body  = '';
    my $first = 1;
    while (1) {
        die "PureLua::Pattern: unterminated character set\n" if $i >= $len;
        my $c = substr($pattern, $i, 1);
        if ($c eq ']' && !$first) { $i++; last; }
        $first = 0;

        if ($c eq '%') {
            my $n = substr($pattern, $i + 1, 1);
            if ($n eq 'd') { $body .= '\d'; $i += 2; next; }
            die "PureLua::Pattern: unsupported pattern class '%$n' inside a set\n" if $n =~ /[A-Za-z]/;
            $body .= quotemeta($n);
            $i += 2;
            next;
        }
        if ($c eq '\\' || $c eq ']') {
            $body .= '\\' . $c;
            $i++;
            next;
        }
        $body .= $c; # literal char, or a bare '-' forming an a-z style range
        $i++;
    }
    return ("[$negate$body]", $i - $start);
}

# Returns (start, end), both 1-based inclusive, or () if no match.
sub find {
    my ($s, $lua_pattern, $init) = @_;
    $init = 1 unless defined $init;
    my $start0 = $init > 0 ? $init - 1 : length($s) + $init;
    $start0 = 0 if $start0 < 0;
    return () if $start0 > length($s);

    my $re  = _compile($lua_pattern);
    my $sub = substr($s, $start0);
    return () unless $sub =~ /($re)/;
    return ( $start0 + $-[0] + 1, $start0 + $+[0] );
}

# Returns the whole matched substring, or undef. (No captures in this
# subset, so match() and find()+substr() agree.)
sub match {
    my ($s, $lua_pattern, $init) = @_;
    my @r = find($s, $lua_pattern, $init);
    return undef unless @r;
    return substr($s, $r[0] - 1, $r[1] - $r[0] + 1);
}

# Returns a 0-arg iterator sub yielding successive whole matches, then
# undef once exhausted - the shape string.gmatch's Lua iterator needs.
sub gmatch_iterator {
    my ($s, $lua_pattern) = @_;
    my $re  = _compile($lua_pattern);
    my $len = length $s;
    my $pos = 0;
    return sub {
        return undef if $pos > $len;
        my $sub = substr($s, $pos);
        return undef unless $sub =~ /($re)/;
        my $abs_start = $pos + $-[0];
        my $abs_end   = $pos + $+[0];
        my $matched   = substr($s, $abs_start, $abs_end - $abs_start);
        $pos = ($abs_end > $abs_start) ? $abs_end : $abs_end + 1;
        return $matched;
    };
}

# $repl is either a plain string (literal replacement) or a coderef
# taking the matched text and returning a replacement string or undef
# (undef keeps the original match - the caller is responsible for
# mapping Lua's "false or nil keeps the match" rule onto that before
# calling in, so this module stays free of Lua-value-specific logic).
# Returns (new_string, substitution_count).
sub gsub {
    my ($s, $lua_pattern, $repl, $max_n) = @_;
    my $re    = _compile($lua_pattern);
    my $len   = length $s;
    my $out   = '';
    my $pos   = 0;
    my $count = 0;

    while ($pos <= $len) {
        last if defined $max_n && $count >= $max_n;
        my $sub = substr($s, $pos);
        last unless $sub =~ /($re)/;
        my $abs_start = $pos + $-[0];
        my $abs_end   = $pos + $+[0];

        $out .= substr($s, $pos, $abs_start - $pos);
        my $matched     = substr($s, $abs_start, $abs_end - $abs_start);
        my $replacement = ref($repl) eq 'CODE' ? $repl->($matched) : $repl;
        $out .= defined($replacement) ? $replacement : $matched;
        $count++;

        if ($abs_end > $abs_start) {
            $pos = $abs_end;
        }
        else {
            $out .= substr($s, $abs_end, 1) if $abs_end < $len;
            $pos = $abs_end + 1;
        }
    }
    $out .= substr($s, $pos) if $pos <= $len;
    return ($out, $count);
}

1;

package PureLua::Cjson;

use strict;
use warnings;


# Native Perl port of this project's own reverse/lua/cjson.lua, exposed
# as the engine's `require "cjson"` module. Operates directly on this
# engine's own value representations (PureLua::Table, the Boolean
# singletons via $eval->lua_bool) rather than re-interpreting the Lua
# source - see .notes/2026-11-16-pure-lua-engine-progress.md for why
# self-hosting that file was rejected (it needs pattern/metatable
# features well outside this engine's documented scope, just to satisfy
# a helper file this project wrote, not the real device-script corpus).

sub encode {
    my ($eval, $v) = @_;
    return _encode_value($eval, $v);
}

sub _encode_value {
    my ($eval, $v) = @_;
    my $ty = $eval->_typename($v);
    return 'null'                    if $ty eq 'nil';
    return $$v ? 'true' : 'false'    if $ty eq 'boolean';
    return $eval->_tostring($v)      if $ty eq 'number';
    return _encode_string($$v)       if $ty eq 'string';
    return _encode_table($eval, $v)  if $ty eq 'table';
    die "PureLua::Cjson: cannot encode a $ty value\n";
}

sub _encode_string {
    my ($s) = @_;
    my $out = $s;
    $out =~ s/([\\"])/\\$1/g;
    $out =~ s/\n/\\n/g;
    $out =~ s/\r/\\r/g;
    $out =~ s/\t/\\t/g;
    $out =~ s/([\x00-\x1f])/sprintf('\\u%04x', ord($1))/ge;
    return '"' . $out . '"';
}

# Array-vs-object detection mirrors cjson.lua's own `is_array`: count all
# entries, then check 1..n are every one of them - a table with extra
# non-sequential keys fails that check and becomes an object instead.
sub _encode_table {
    my ($eval, $t) = @_;
    my $list = $t->pairs_list;
    my $n    = scalar @$list;

    my $is_array = 1;
    for my $i (1 .. $n) {
        if (!defined $t->get($i)) { $is_array = 0; last; }
    }

    if ($is_array) {
        return '[' . join( ',', map { _encode_value($eval, $t->get($_)) } 1 .. $n ) . ']';
    }
    return '{' . join( ',', map {
        _encode_string($eval->_tostring($_->[0])) . ':' . _encode_value($eval, $_->[1])
    } @$list ) . '}';
}

sub decode {
    my ($eval, $json) = @_;
    my ($value) = _parse_value($eval, $json, 0);
    return $value;
}

sub _skip_ws {
    my ($s, $i) = @_;
    $i++ while $i < length($s) && substr($s, $i, 1) =~ /\s/;
    return $i;
}

sub _parse_value {
    my ($eval, $s, $i) = @_;
    $i = _skip_ws($s, $i);
    my $c = substr($s, $i, 1);
    if ($c eq '"') {
        my ($text, $j) = _parse_string($s, $i);
        return ( $eval->lua_str($text), $j );
    }
    return _parse_object($eval, $s, $i) if $c eq '{';
    return _parse_array($eval, $s, $i)  if $c eq '[';
    return ( $eval->lua_bool(1), $i + 4 ) if substr($s, $i, 4) eq 'true';
    return ( $eval->lua_bool(0), $i + 5 ) if substr($s, $i, 5) eq 'false';
    return ( undef, $i + 4 )              if substr($s, $i, 4) eq 'null';
    return _parse_number($s, $i);
}

my %ESCAPE_MAP = ( n => "\n", r => "\r", t => "\t", b => "\b", f => "\f", '"' => '"', '\\' => '\\', '/' => '/' );

sub _parse_string {
    my ($s, $i) = @_;
    die "PureLua::Cjson: expected a string at position $i\n" unless substr($s, $i, 1) eq '"';
    my $j   = $i + 1;
    my $out = '';
    while (1) {
        die "PureLua::Cjson: unterminated string\n" if $j >= length($s);
        my $c = substr($s, $j, 1);
        if ($c eq '"') { return ( $out, $j + 1 ); }
        if ($c eq '\\') {
            my $e = substr($s, $j + 1, 1);
            if (exists $ESCAPE_MAP{$e}) { $out .= $ESCAPE_MAP{$e}; $j += 2; }
            elsif ($e eq 'u') {
                my $hex = substr($s, $j + 2, 4);
                $out .= chr( hex($hex) & 0xFF ); # byte-level, matching cjson.lua's own `% 256`
                $j += 6;
            }
            else { $out .= $e; $j += 2; }
            next;
        }
        $out .= $c;
        $j++;
    }
}

sub _parse_number {
    my ($s, $i) = @_;
    substr($s, $i) =~ /\A-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/
        or die "PureLua::Cjson: malformed number at position $i\n";
    my $num = $&;
    return ( $num + 0, $i + length($num) );
}

sub _parse_array {
    my ($eval, $s, $i) = @_;
    my $t = PureLua::Table->new;
    $i = _skip_ws($s, $i + 1);
    return ( $t, $i + 1 ) if substr($s, $i, 1) eq ']';

    my $idx = 1;
    while (1) {
        my ($v, $j) = _parse_value($eval, $s, $i);
        $t->set($idx++, $v);
        $i = _skip_ws($s, $j);
        my $c = substr($s, $i, 1);
        if ($c eq ',') { $i = _skip_ws($s, $i + 1); next; }
        return ( $t, $i + 1 ) if $c eq ']';
        die "PureLua::Cjson: expected ',' or ']' at position $i\n";
    }
}

sub _parse_object {
    my ($eval, $s, $i) = @_;
    my $t = PureLua::Table->new;
    $i = _skip_ws($s, $i + 1);
    return ( $t, $i + 1 ) if substr($s, $i, 1) eq '}';

    while (1) {
        my ($k, $j) = _parse_string($s, $i);
        $i = _skip_ws($s, $j);
        die "PureLua::Cjson: expected ':' at position $i\n" unless substr($s, $i, 1) eq ':';
        my ($v, $j2) = _parse_value($eval, $s, _skip_ws($s, $i + 1));
        $t->set($eval->lua_str($k), $v);
        $i = _skip_ws($s, $j2);
        my $c = substr($s, $i, 1);
        if ($c eq ',') { $i = _skip_ws($s, $i + 1); next; }
        return ( $t, $i + 1 ) if $c eq '}';
        die "PureLua::Cjson: expected ',' or '}' at position $i\n";
    }
}

1;

package PureLua::Parser;

use strict;
use warnings;


# Recursive-descent parser for the Lua 5.1 grammar (reference manual §8),
# producing a plain-hashref AST. No goto/labels (5.2+ only, not in the 5.1
# grammar at all). Metatables/coroutines are library identifiers, not
# syntax, so they need nothing special here.

# Binary operator precedence: { op => [left_binding, right_binding] }.
# Higher number binds tighter. right_binding < left_binding makes an
# operator right-associative (lets the same operator recurse on the
# right); right_binding == left_binding makes it left-associative (the
# caller's own while-loop picks up the next same-level operator instead).
my %BINOP = (
    'or' => [1, 1],
    'and' => [2, 2],
    '<' => [3, 3], '>' => [3, 3], '<=' => [3, 3], '>=' => [3, 3],
    '==' => [3, 3], '~=' => [3, 3],
    '..' => [4, 3],
    '+' => [5, 5], '-' => [5, 5],
    '*' => [6, 6], '/' => [6, 6], '%' => [6, 6],
    '^' => [8, 7],
);

# Binding power used when parsing a unary operator's operand: high enough
# to exclude everything except '^', which binds tighter than unary minus
# (so `-x^2` parses as `-(x^2)`).
my $UNARY_PREC = 7;

my %BLOCK_END_KEYWORDS = map { $_ => 1 } qw(end else elseif until);

sub parse {
    my ($class, $src_or_tokens) = @_;
    my $tokens = ref($src_or_tokens) eq 'ARRAY'
        ? $src_or_tokens
        : PureLua::Lexer->tokenize($src_or_tokens);
    my $self = bless { tokens => $tokens, idx => 0 }, $class;

    my $body = $self->_parse_block;
    $self->_expect('eof');
    return { type => 'chunk', body => $body };
}

# --- token cursor helpers ---------------------------------------------

sub _cur  { my ($self, $off) = @_; return $self->{tokens}[$self->{idx} + ($off || 0)]; }
sub _check    { my ($self, $type) = @_; return $self->_cur->{type} eq $type; }
sub _check_kw { my ($self, $kw)   = @_; my $t = $self->_cur; return $t->{type} eq 'keyword' && $t->{value} eq $kw; }

sub _advance {
    my ($self) = @_;
    my $t = $self->_cur;
    $self->{idx}++;
    return $t;
}

sub _error {
    my ($self, $msg) = @_;
    my $t = $self->_cur;
    my $got = $t->{type} eq 'eof' ? 'eof' : ($t->{value} // $t->{type});
    die "PureLua::Parser: $msg (got '$got') at line $t->{line}\n";
}

sub _expect {
    my ($self, $type) = @_;
    $self->_error("expected '$type'") unless $self->_check($type);
    return $self->_advance;
}

sub _expect_kw {
    my ($self, $kw) = @_;
    $self->_error("expected '$kw'") unless $self->_check_kw($kw);
    return $self->_advance;
}

sub _expect_name {
    my ($self) = @_;
    $self->_error('expected identifier') unless $self->_check('name');
    return $self->_advance->{value};
}

sub _at_block_end {
    my ($self) = @_;
    my $t = $self->_cur;
    return 1 if $t->{type} eq 'eof';
    return 1 if $t->{type} eq 'keyword' && $BLOCK_END_KEYWORDS{ $t->{value} };
    return 0;
}

sub _at_laststat {
    my ($self) = @_;
    my $t = $self->_cur;
    return $t->{type} eq 'keyword' && ($t->{value} eq 'return' || $t->{value} eq 'break');
}

# --- statements ---------------------------------------------------------

sub _parse_block {
    my ($self) = @_;
    my @stmts;
    while (!$self->_at_block_end) {
        if ($self->_check(';')) {
            $self->_advance;
            next;
        }
        if ($self->_at_laststat) {
            push @stmts, $self->_parse_laststat;
            $self->_advance if $self->_check(';');
            last;
        }
        push @stmts, $self->_parse_statement;
        $self->_advance if $self->_check(';');
    }
    return \@stmts;
}

sub _parse_laststat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    if ($self->_check_kw('break')) {
        $self->_advance;
        return { type => 'break', line => $line };
    }
    $self->_expect_kw('return');
    my $exprs = [];
    unless ($self->_check(';') || $self->_at_block_end) {
        $exprs = $self->_parse_explist;
    }
    return { type => 'return', exprs => $exprs, line => $line };
}

sub _parse_statement {
    my ($self) = @_;
    my $line = $self->_cur->{line};

    return $self->_parse_if       if $self->_check_kw('if');
    return $self->_parse_while    if $self->_check_kw('while');
    return $self->_parse_repeat   if $self->_check_kw('repeat');
    return $self->_parse_for      if $self->_check_kw('for');
    return $self->_parse_function_stat if $self->_check_kw('function');
    return $self->_parse_local    if $self->_check_kw('local');

    if ($self->_check_kw('do')) {
        $self->_advance;
        my $body = $self->_parse_block;
        $self->_expect_kw('end');
        return { type => 'do', body => $body, line => $line };
    }

    return $self->_parse_exprstat;
}

sub _parse_if {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('if');
    my @clauses;
    my $cond = $self->_parse_expr(0);
    $self->_expect_kw('then');
    my $body = $self->_parse_block;
    push @clauses, { cond => $cond, body => $body };

    while ($self->_check_kw('elseif')) {
        $self->_advance;
        my $c = $self->_parse_expr(0);
        $self->_expect_kw('then');
        my $b = $self->_parse_block;
        push @clauses, { cond => $c, body => $b };
    }

    my $else_body;
    if ($self->_check_kw('else')) {
        $self->_advance;
        $else_body = $self->_parse_block;
    }
    $self->_expect_kw('end');
    return { type => 'if', clauses => \@clauses, else_body => $else_body, line => $line };
}

sub _parse_while {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('while');
    my $cond = $self->_parse_expr(0);
    $self->_expect_kw('do');
    my $body = $self->_parse_block;
    $self->_expect_kw('end');
    return { type => 'while', cond => $cond, body => $body, line => $line };
}

sub _parse_repeat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('repeat');
    my $body = $self->_parse_block;
    $self->_expect_kw('until');
    my $cond = $self->_parse_expr(0);
    return { type => 'repeat', body => $body, cond => $cond, line => $line };
}

sub _parse_for {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('for');
    my $name1 = $self->_expect_name;

    if ($self->_check('=')) {
        $self->_advance;
        my $start = $self->_parse_expr(0);
        $self->_expect(',');
        my $stop = $self->_parse_expr(0);
        my $step;
        if ($self->_check(',')) {
            $self->_advance;
            $step = $self->_parse_expr(0);
        }
        $self->_expect_kw('do');
        my $body = $self->_parse_block;
        $self->_expect_kw('end');
        return { type => 'fornum', var => $name1, start => $start, stop => $stop, step => $step, body => $body, line => $line };
    }

    my @names = ($name1);
    while ($self->_check(',')) {
        $self->_advance;
        push @names, $self->_expect_name;
    }
    $self->_expect_kw('in');
    my $exprs = $self->_parse_explist;
    $self->_expect_kw('do');
    my $body = $self->_parse_block;
    $self->_expect_kw('end');
    return { type => 'forin', names => \@names, exprs => $exprs, body => $body, line => $line };
}

sub _parse_function_stat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('function');

    my $target = { type => 'name', name => $self->_expect_name };
    while ($self->_check('.')) {
        $self->_advance;
        my $field = $self->_expect_name;
        $target = { type => 'index', obj => $target, key => { type => 'string', value => $field } };
    }

    my $is_method = 0;
    if ($self->_check(':')) {
        $self->_advance;
        my $field = $self->_expect_name;
        $target = { type => 'index', obj => $target, key => { type => 'string', value => $field } };
        $is_method = 1;
    }

    my $body = $self->_parse_funcbody($is_method);
    return { type => 'function', target => $target, body => $body, line => $line };
}

sub _parse_funcbody {
    my ($self, $is_method) = @_;
    $self->_expect('(');
    my @params = $is_method ? ('self') : ();
    my $vararg = 0;

    unless ($self->_check(')')) {
        while (1) {
            if ($self->_check('...')) {
                $self->_advance;
                $vararg = 1;
                last;
            }
            push @params, $self->_expect_name;
            if ($self->_check(',')) {
                $self->_advance;
                next;
            }
            last;
        }
    }
    $self->_expect(')');
    my $body = $self->_parse_block;
    $self->_expect_kw('end');
    return { params => \@params, vararg => $vararg, body => $body };
}

sub _parse_local {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('local');

    if ($self->_check_kw('function')) {
        $self->_advance;
        my $name = $self->_expect_name;
        my $body = $self->_parse_funcbody(0);
        return { type => 'localfunction', name => $name, body => $body, line => $line };
    }

    my @names = ($self->_expect_name);
    while ($self->_check(',')) {
        $self->_advance;
        push @names, $self->_expect_name;
    }
    my $exprs = [];
    if ($self->_check('=')) {
        $self->_advance;
        $exprs = $self->_parse_explist;
    }
    return { type => 'local', names => \@names, exprs => $exprs, line => $line };
}

# varlist '=' explist, or a bare functioncall used as a statement.
sub _parse_exprstat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    my $first = $self->_parse_suffixedexp;

    if ($self->_check('=') || $self->_check(',')) {
        my @targets = ($first);
        while ($self->_check(',')) {
            $self->_advance;
            push @targets, $self->_parse_suffixedexp;
        }
        for my $t (@targets) {
            $self->_error('cannot assign to this expression')
                unless $t->{type} eq 'name' || $t->{type} eq 'index';
        }
        $self->_expect('=');
        my $exprs = $self->_parse_explist;
        return { type => 'assign', targets => \@targets, exprs => $exprs, line => $line };
    }

    $self->_error('syntax error: expression used as a statement')
        unless $first->{type} eq 'call' || $first->{type} eq 'methodcall';
    return { type => 'callstat', call => $first, line => $line };
}

# --- expressions ---------------------------------------------------------

sub _is_unop {
    my ($self) = @_;
    my $t = $self->_cur;
    return 1 if $t->{type} eq '-' || $t->{type} eq '#';
    return 1 if $t->{type} eq 'keyword' && $t->{value} eq 'not';
    return 0;
}

sub _binop_key {
    my ($self) = @_;
    my $t = $self->_cur;
    if ($t->{type} eq 'keyword') {
        return $t->{value} if $t->{value} eq 'and' || $t->{value} eq 'or';
        return undef;
    }
    return $t->{type};
}

sub _parse_expr {
    my ($self, $limit) = @_;
    my $left;

    if ($self->_is_unop) {
        my $t = $self->_advance;
        my $op = $t->{type} eq 'keyword' ? $t->{value} : $t->{type};
        my $operand = $self->_parse_expr($UNARY_PREC);
        $left = { type => 'unop', op => $op, expr => $operand, line => $t->{line} };
    }
    else {
        $left = $self->_parse_simple_expr;
    }

    while (1) {
        my $key = $self->_binop_key;
        last unless defined $key && exists $BINOP{$key};
        my ($lprec, $rprec) = @{ $BINOP{$key} };
        last if $lprec <= $limit;
        my $line = $self->_cur->{line};
        $self->_advance;
        my $right = $self->_parse_expr($rprec);
        $left = { type => 'binop', op => $key, lhs => $left, rhs => $right, line => $line };
    }
    return $left;
}

sub _parse_simple_expr {
    my ($self) = @_;
    my $t = $self->_cur;

    if ($t->{type} eq 'number') { $self->_advance; return { type => 'number', value => $t->{value} }; }
    if ($t->{type} eq 'string') { $self->_advance; return { type => 'string', value => $t->{value} }; }
    if ($t->{type} eq '...')    { $self->_advance; return { type => 'vararg' }; }
    if ($t->{type} eq '{')      { return $self->_parse_table; }

    if ($t->{type} eq 'keyword') {
        if ($t->{value} eq 'nil')   { $self->_advance; return { type => 'nil' }; }
        if ($t->{value} eq 'true')  { $self->_advance; return { type => 'true' }; }
        if ($t->{value} eq 'false') { $self->_advance; return { type => 'false' }; }
        if ($t->{value} eq 'function') {
            $self->_advance;
            my $body = $self->_parse_funcbody(0);
            return { type => 'function', body => $body };
        }
    }

    if ($t->{type} eq 'name' || $t->{type} eq '(') {
        return $self->_parse_suffixedexp;
    }

    $self->_error('unexpected token in expression');
}

sub _parse_primaryexp {
    my ($self) = @_;
    if ($self->_check('name')) {
        return { type => 'name', name => $self->_advance->{value} };
    }
    if ($self->_check('(')) {
        $self->_advance;
        my $e = $self->_parse_expr(0);
        $self->_expect(')');
        return { type => 'paren', expr => $e };
    }
    $self->_error("expected identifier or '('");
}

# prefixexp with the full suffix chain: .Name, [exp], :Name args, args.
sub _parse_suffixedexp {
    my ($self) = @_;
    my $base = $self->_parse_primaryexp;
    while (1) {
        my $line = $self->_cur->{line};
        if ($self->_check('.')) {
            $self->_advance;
            my $field = $self->_expect_name;
            $base = { type => 'index', obj => $base, key => { type => 'string', value => $field }, line => $line };
        }
        elsif ($self->_check('[')) {
            $self->_advance;
            my $key = $self->_parse_expr(0);
            $self->_expect(']');
            $base = { type => 'index', obj => $base, key => $key, line => $line };
        }
        elsif ($self->_check(':')) {
            $self->_advance;
            my $method = $self->_expect_name;
            my $args = $self->_parse_args;
            $base = { type => 'methodcall', obj => $base, method => $method, args => $args, line => $line };
        }
        elsif ($self->_check('(') || $self->_check('string') || $self->_check('{')) {
            my $args = $self->_parse_args;
            $base = { type => 'call', func => $base, args => $args, line => $line };
        }
        else {
            last;
        }
    }
    return $base;
}

sub _parse_args {
    my ($self) = @_;
    if ($self->_check('(')) {
        $self->_advance;
        my $args = $self->_check(')') ? [] : $self->_parse_explist;
        $self->_expect(')');
        return $args;
    }
    if ($self->_check('string')) {
        return [ { type => 'string', value => $self->_advance->{value} } ];
    }
    if ($self->_check('{')) {
        return [ $self->_parse_table ];
    }
    $self->_error('expected function arguments');
}

sub _parse_table {
    my ($self) = @_;
    $self->_expect('{');
    my @fields;
    while (!$self->_check('}')) {
        if ($self->_check('[')) {
            $self->_advance;
            my $key = $self->_parse_expr(0);
            $self->_expect(']');
            $self->_expect('=');
            my $value = $self->_parse_expr(0);
            push @fields, { type => 'keyed', key => $key, value => $value };
        }
        elsif ($self->_check('name') && $self->_cur(1)->{type} eq '=') {
            my $name = $self->_advance->{value};
            $self->_advance; # '='
            my $value = $self->_parse_expr(0);
            push @fields, { type => 'named', key => $name, value => $value };
        }
        else {
            my $value = $self->_parse_expr(0);
            push @fields, { type => 'pos', value => $value };
        }

        if ($self->_check(',') || $self->_check(';')) {
            $self->_advance;
        }
        else {
            last;
        }
    }
    $self->_expect('}');
    return { type => 'table', fields => \@fields };
}

sub _parse_explist {
    my ($self) = @_;
    my @exprs = ($self->_parse_expr(0));
    while ($self->_check(',')) {
        $self->_advance;
        push @exprs, $self->_parse_expr(0);
    }
    return \@exprs;
}

1;

package PureLua::Stdlib;

use strict;
use warnings;


# Installs exactly the stdlib surface the design doc's grep of the real
# device scripts found (.notes/2026-11-16-pure-perl-lua-engine-design.md):
# math.random/randomseed/floor/modf, os.time, string.char/sub/format/
# gsub/rep/match/find/gmatch, table.insert, pairs, tonumber, tostring,
# bit.band/bor/bxor/lshift/rshift/bnot. Nothing beyond that list -
# `require "cjson"` is deliberately NOT here (see progress notes: the
# project's own cjson.lua shim uses string method-call syntax and
# pattern features - anchors, %c, ?-quantifiers - well outside this
# engine's documented scope, so wiring it up is deferred until actually
# needed, not pre-built speculatively).

sub install {
    my ($class, $eval) = @_;
    my $g = $eval->{globals};

    $g->set(PureLua::Table::wrap_str('tonumber'), sub {
        my ($v, $base) = @_;
        return $eval->_tonumber($v) unless defined $base;
        return _tonumber_base($eval, $eval->str_value($v), int($eval->_tonumber($base)));
    });
    $g->set(PureLua::Table::wrap_str('tostring'), sub { my ($v) = @_; return $eval->lua_str($eval->_tostring($v)); });
    $g->set(PureLua::Table::wrap_str('type'),     sub { my ($v) = @_; return $eval->lua_str($eval->_typename($v)); });
    $g->set(PureLua::Table::wrap_str('print'),    sub { return; }); # debug output only; not consumed by anything downstream.
    $g->set(PureLua::Table::wrap_str('pairs'),    sub { my ($t) = @_; return _pairs($t); });
    $g->set(PureLua::Table::wrap_str('require'),  sub { my ($name) = @_; return _require($eval, $name); });
    $g->set(PureLua::Table::wrap_str('pcall'),    sub {
        my ($fn, @args) = @_;
        my @r = eval { $eval->_call_value($fn, \@args) };
        if (my $err = $@) {
            $err =~ s/\n\z//;
            return ( $eval->lua_bool(0), $eval->lua_str($err) );
        }
        return ( $eval->lua_bool(1), @r );
    });

    $g->set(PureLua::Table::wrap_str('math'),   _build_math($eval));
    $g->set(PureLua::Table::wrap_str('os'),     _build_os());
    $g->set(PureLua::Table::wrap_str('bit'),    _build_bit($eval));
    $g->set(PureLua::Table::wrap_str('string'), _build_string($eval));
    $g->set(PureLua::Table::wrap_str('table'),  _build_table());

    return;
}

my %MODULES = ( cjson => \&_cjson_module, bit => \&_build_bit );

sub _require {
    my ($eval, $name) = @_;
    $name = $eval->str_value($name);
    my $builder = $MODULES{$name};
    die "PureLua::Stdlib: module '$name' is not available (no general module loader - only 'cjson' is built in)\n"
        unless $builder;
    return $builder->($eval);
}

sub _cjson_module {
    my ($eval) = @_;
    my $m = PureLua::Table->new;
    $m->set(PureLua::Table::wrap_str('encode'), sub { my ($v) = @_; return $eval->lua_str( PureLua::Cjson::encode($eval, $v) ); });
    $m->set(PureLua::Table::wrap_str('decode'), sub { my ($s) = @_; return PureLua::Cjson::decode($eval, $eval->str_value($s)); });
    return $m;
}

# Lua's tonumber(s, base): the real device script relies on this to
# parse hex byte pairs (tonumber(doublebytestr, 16)) - a plain-decimal
# parse happened to give the right answer for digit-only pairs ("02")
# but silently returned nil for actual hex letters ("aa"), which emptied
# that table slot entirely (nil assignment deletes a key). Found by
# cross-checking the real device script's dataToJson against luajit.
sub _tonumber_base {
    my ($eval, $s, $base) = @_;
    return undef unless defined $s;
    $s =~ s/\A\s+|\s+\z//g;
    return undef unless length $s;
    my $neg = $s =~ s/\A-//;
    $s =~ s/\A\+//;
    return undef unless length $s;
    my $n = 0;
    for my $c (split //, lc $s) {
        my $d = $c =~ /[0-9]/ ? ord($c) - ord('0')
              : $c =~ /[a-z]/ ? ord($c) - ord('a') + 10
              : return undef;
        return undef if $d >= $base;
        $n = $n * $base + $d;
    }
    return $neg ? -$n : $n;
}

sub _pairs {
    my ($t) = @_;
    die "PureLua::Stdlib: bad argument to 'pairs' (table expected)\n" unless ref($t) eq 'PureLua::Table';
    my $list = $t->pairs_list;
    my $i = 0;
    my $iter = sub {
        return () if $i > $#$list;
        my $pair = $list->[ $i++ ];
        return ( $pair->[0], $pair->[1] );
    };
    return ( $iter, $t, undef );
}

sub _build_math {
    my ($eval) = @_;
    my $math = PureLua::Table->new;
    $math->set(PureLua::Table::wrap_str('floor'), sub { my ($x) = @_; return $eval->_lua_floor($eval->_tonumber($x)); });
    $math->set(PureLua::Table::wrap_str('modf'), sub {
        my ($x) = @_;
        my $n  = $eval->_tonumber($x);
        my $ip = int($n); # Perl's int() truncates toward zero, same as C's modf.
        return ( $ip, $n - $ip );
    });
    $math->set(PureLua::Table::wrap_str('random'), sub {
        my @a = @_;
        return rand() if @a == 0;
        if (@a == 1) {
            my $m = int($eval->_tonumber($a[0]));
            return 1 + int(rand($m));
        }
        my ($m, $n) = ( int($eval->_tonumber($a[0])), int($eval->_tonumber($a[1])) );
        return $m + int(rand($n - $m + 1));
    });
    $math->set(PureLua::Table::wrap_str('fmod'), sub {
        my ($a, $b) = @_;
        $a = $eval->_tonumber($a);
        $b = $eval->_tonumber($b);
        return $a - int($a / $b) * $b; # C fmod semantics: remainder keeps the dividend's sign.
    });
    $math->set(PureLua::Table::wrap_str('randomseed'), sub {
        my ($x) = @_;
        srand(defined $x ? $eval->_tonumber($x) : time());
        return;
    });
    return $math;
}

sub _build_os {
    my $os = PureLua::Table->new;
    $os->set(PureLua::Table::wrap_str('time'), sub { return time(); });
    return $os;
}

# Real LuaJIT's bit library errors loudly on a non-number argument
# ("bad argument #N to 'bor' (number expected, got nil)") rather than
# silently treating it as 0 - matching that strictness turns a script
# bug (or a wrong BASE_CONTROL probe) into the same loud failure real
# Lua would give, instead of a quietly-different "succeeded" result.
# Delegates numeric coercion to $eval->_tonumber rather than its own
# regex+int() so a hex-string value ("0x2010") is parsed via hex(), not
# silently truncated to 0 by Perl's own "+0"/int() (which don't
# understand a "0x" prefix) - the same class of bug _tonumber itself
# had until the wider corpus sweep caught it.
sub _u32 {
    my ($eval, $x) = @_;
    my $n = $eval->_tonumber($x);
    die "bad argument to bit operation (number expected, got " . $eval->_typename($x) . ")\n"
        unless defined $n;
    return int($n) & 0xFFFFFFFF;
}
sub _s32 { my ($x) = @_; return ( $x >= 0x80000000 ) ? $x - 4_294_967_296 : $x; }

sub _build_bit {
    my ($eval) = @_;
    my $bit = PureLua::Table->new;
    $bit->set(PureLua::Table::wrap_str('band'), sub { my $r = 0xFFFFFFFF; $r &= _u32($eval, $_) for @_; return _s32($r); });
    $bit->set(PureLua::Table::wrap_str('bor'),  sub { my $r = 0;          $r |= _u32($eval, $_) for @_; return _s32($r); });
    $bit->set(PureLua::Table::wrap_str('bxor'), sub { my $r = 0;          $r ^= _u32($eval, $_) for @_; return _s32($r); });
    $bit->set(PureLua::Table::wrap_str('bnot'), sub { my ($x) = @_; return _s32( ( ~_u32($eval, $x) ) & 0xFFFFFFFF ); });
    $bit->set(PureLua::Table::wrap_str('lshift'), sub {
        my ($x, $n) = @_;
        return _s32( ( _u32($eval, $x) << ( _u32($eval, $n) & 31 ) ) & 0xFFFFFFFF );
    });
    $bit->set(PureLua::Table::wrap_str('rshift'), sub {
        my ($x, $n) = @_;
        return _s32( ( _u32($eval, $x) >> ( _u32($eval, $n) & 31 ) ) & 0xFFFFFFFF );
    });
    return $bit;
}

sub _str_sub {
    my ($s, $i, $j) = @_;
    my $len = length $s;
    $i = defined $i ? int($i) : 1;
    $j = defined $j ? int($j) : -1;
    $i = $len + $i + 1 if $i < 0;
    $j = $len + $j + 1 if $j < 0;
    $i = 1 if $i < 1;
    $j = $len if $j > $len;
    return '' if $i > $j;
    return substr($s, $i - 1, $j - $i + 1);
}

# Lua's %q: wraps a string in double quotes, escaping the characters
# that would otherwise break re-reading it back as a Lua literal.
sub _format_q {
    my ($v) = @_;
    return 'nil' unless defined $v;
    my $s = ref($v) eq 'PureLua::Str' ? $$v : $v;
    $s =~ s/([\\"])/\\$1/g;
    $s =~ s/\n/\\\n/g;
    $s =~ s/\r/\\r/g;
    $s =~ s/\0/\\0/g;
    return '"' . $s . '"';
}

# Narrow: handles whatever conversion letter is asked for by handing a
# single directive straight to Perl's own sprintf (close enough to
# Lua's for %d/%s/%x/%02x etc.), except %q which Lua defines specially.
sub _format {
    my ($fmt, $args) = @_;
    $fmt = $$fmt if ref($fmt) eq 'PureLua::Str';
    my $out = '';
    my $ai  = 0;
    my $i   = 0;
    my $len = length $fmt;

    while ($i < $len) {
        my $c = substr($fmt, $i, 1);
        if ($c ne '%') { $out .= $c; $i++; next; }
        if (substr($fmt, $i + 1, 1) eq '%') { $out .= '%'; $i += 2; next; }

        my $rest = substr($fmt, $i);
        $rest =~ /\A%([-+0 #]*)(\d*)(\.\d+)?([a-zA-Z])/
            or die "PureLua::Stdlib: malformed format directive in '$fmt' at position $i\n";
        my $directive = $&;
        my $conv       = $4;
        $i += length($directive);
        my $arg = $args->[ $ai++ ];

        if ($conv eq 'q') {
            $out .= _format_q($arg);
        }
        else {
            $arg = $$arg if ref($arg) eq 'PureLua::Str';
            $out .= sprintf($directive, $arg);
        }
    }
    return $out;
}

sub _build_string {
    my ($eval) = @_;
    my $str = PureLua::Table->new;

    my $sv = sub { return $eval->str_value( $_[0] ); }; # unwrap a Lua string arg to plain Perl text

    $str->set(PureLua::Table::wrap_str('char'),    sub { return $eval->lua_str( join( '', map { chr(int($_)) } @_ ) ); });
    $str->set(PureLua::Table::wrap_str('sub'),     sub { my ($s, $i, $j) = @_; return $eval->lua_str( _str_sub($sv->($s), $i, $j) ); });
    $str->set(PureLua::Table::wrap_str('rep'), sub {
        my ($s, $n) = @_;
        $s = $sv->($s);
        $n = int($n);
        return $eval->lua_str( $n > 0 ? ( $s x $n ) : '' );
    });
    $str->set(PureLua::Table::wrap_str('reverse'), sub { my ($s) = @_; return $eval->lua_str( scalar reverse $sv->($s) ); });
    $str->set(PureLua::Table::wrap_str('format'),  sub { my ($fmt, @args) = @_; return $eval->lua_str( _format($fmt, \@args) ); });
    $str->set(PureLua::Table::wrap_str('byte'), sub {
        my ($s, $i, $j) = @_;
        $s = $sv->($s);
        $i = defined $i ? int($i) : 1;
        $j = defined $j ? int($j) : $i;
        my $len = length $s;
        $i = $len + $i + 1 if $i < 0;
        $j = $len + $j + 1 if $j < 0;
        $i = 1 if $i < 1;
        $j = $len if $j > $len;
        return () if $i > $j;
        return map { ord( substr($s, $_ - 1, 1) ) } $i .. $j;
    });

    $str->set(PureLua::Table::wrap_str('find'), sub {
        my ($s, $p, $init) = @_;
        return PureLua::Pattern::find($sv->($s), $sv->($p), $init);
    });
    $str->set(PureLua::Table::wrap_str('match'), sub {
        my ($s, $p, $init) = @_;
        my $m = PureLua::Pattern::match($sv->($s), $sv->($p), $init);
        return defined $m ? $eval->lua_str($m) : ();
    });
    $str->set(PureLua::Table::wrap_str('gmatch'), sub {
        my ($s, $p) = @_;
        my $it = PureLua::Pattern::gmatch_iterator($sv->($s), $sv->($p));
        return sub { my $m = $it->(); return defined $m ? $eval->lua_str($m) : (); };
    });
    $str->set(PureLua::Table::wrap_str('gsub'), sub {
        my ($s, $p, $repl, $n) = @_;
        my $perl_repl = ( ref($repl) eq 'PureLua::Function' || ref($repl) eq 'CODE' )
            ? sub {
                my ($matched) = @_;
                my @r  = $eval->_call_value($repl, [ $eval->lua_str($matched) ]);
                my $rv = $r[0];
                return PureLua::Eval::_is_truthy($rv) ? $eval->_tostring($rv) : undef;
            }
            : $sv->($repl);
        my ($out, $count) = PureLua::Pattern::gsub($sv->($s), $sv->($p), $perl_repl, $n);
        return ( $eval->lua_str($out), $count );
    });
    return $str;
}

sub _build_table {
    my $table = PureLua::Table->new;
    $table->set(PureLua::Table::wrap_str('insert'), sub {
        my ($t, @rest) = @_;
        if (@rest == 1) {
            $t->set($t->lua_length + 1, $rest[0]);
        }
        else {
            my ($pos, $val) = @rest;
            for (my $i = $t->lua_length; $i >= $pos; $i--) {
                $t->set($i + 1, $t->get($i));
            }
            $t->set($pos, $val);
        }
        return;
    });
    return $table;
}

1;

package PureLua::Eval;

use strict;
use warnings;


# Lua's truthiness excludes only nil and false - 0 and "" are truthy, so
# booleans need a representation distinct from Perl's own notion of
# false. Blessed singleton scalars, checked by ref(), carry that.
my $TRUE  = bless( \( my $t = 1 ), 'PureLua::Boolean' );
my $FALSE = bless( \( my $f = 0 ), 'PureLua::Boolean' );

# Lua strings are blessed scalar refs; Lua numbers stay plain,
# unblessed Perl scalars. This split exists because Perl itself can't
# otherwise tell "the string '00'" apart from "the number 0" - without
# it, a numeric-looking string fed back through _tostring() (e.g. during
# `..` concatenation) gets silently re-formatted as a number, which
# corrupted real protocol hex strings (string.format("%02x", 0) -> "00",
# then "s..that" collapsed it back down to "0", dropping the leading
# zero - found while cross-checking the real device script against
# luajit; see .notes/2026-11-16-pure-lua-engine-progress.md).
sub _mkstr { my ($s) = @_; return PureLua::Table::wrap_str($s); }

sub _is_truthy {
    my ($v) = @_;
    return 0 unless defined $v;
    return 0 if ref($v) eq 'PureLua::Boolean' && !$$v;
    return 1;
}

sub _bool { return $_[0] ? $TRUE : $FALSE; }

# Control-flow signals, thrown via die and caught at the matching
# boundary (function call for Return, loop body for Break) - the
# standard way to unwind a tree-walking interpreter through arbitrarily
# nested blocks without threading a signal value through every call site.
package PureLua::Signal::Return;
sub new { my ($class, $values) = @_; return bless { values => $values }, $class; }

package PureLua::Signal::Break;
sub new { return bless {}, shift; }

package PureLua::Eval;

sub new {
    my ($class) = @_;
    return bless { globals => PureLua::Table->new }, $class;
}

# Public: lets sibling modules (e.g. PureLua::Cjson, decoding JSON
# true/false) build a real Lua boolean without reaching into this
# module's private $TRUE/$FALSE singletons directly.
sub lua_bool { my ($self, $truthy) = @_; return _bool($truthy); }

# Public: wrap a plain Perl string as a real Lua string value, and
# unwrap one back to plain Perl text (passes non-Str scalars through
# unchanged, so callers can use it defensively on an already-plain
# value without checking first).
sub lua_str   { my ($self, $s) = @_; return _mkstr($s); }
sub str_value { my ($self, $v) = @_; return ref($v) eq 'PureLua::Str' ? $$v : $v; }

# --- scope / environment -------------------------------------------------
#
# Each scope is {vars=>{name=>\$scalar}, parent=>$scope_or_undef}. A
# fresh scalar ref per `local` declaration is what makes closures over
# loop variables work: each loop iteration gets its own cell, so
# closures created in different iterations don't alias each other
# (real Lua 5.1 semantics - see t/eval.t's "fresh loop variable" case).
# Globals are not scope entries at all - they live in one shared Table,
# exactly as real Lua desugars a global reference to `_G.name`.

sub _new_scope {
    my ($self, $parent) = @_;
    return { vars => {}, parent => $parent };
}

sub _declare_local {
    my ($self, $scope, $name, $value) = @_;
    $scope->{vars}{$name} = \$value;
}

sub _lookup_cell {
    my ($self, $scope, $name) = @_;
    while ($scope) {
        return $scope->{vars}{$name} if exists $scope->{vars}{$name};
        $scope = $scope->{parent};
    }
    return undef;
}

sub _get_var {
    my ($self, $scope, $name) = @_;
    my $cell = $self->_lookup_cell($scope, $name);
    return $cell ? $$cell : $self->{globals}->get(PureLua::Table::wrap_str($name));
}

sub _set_var {
    my ($self, $scope, $name, $value) = @_;
    my $cell = $self->_lookup_cell($scope, $name);
    if ($cell) { $$cell = $value; }
    else       { $self->{globals}->set(PureLua::Table::wrap_str($name), $value); }
}

sub _get_varargs {
    my ($self, $scope) = @_;
    while ($scope) {
        return $scope->{varargs} if exists $scope->{varargs};
        $scope = $scope->{parent};
    }
    return [];
}

sub _runtime_error {
    my ($self, $msg, $line) = @_;
    die "PureLua::Eval: $msg" . (defined $line ? " at line $line" : '') . "\n";
}

# --- public entry point ---------------------------------------------------

# Runs a parsed chunk's top-level block, returning whatever its
# top-level `return` yields (empty list if it falls off the end).
sub run {
    my ($self, $chunk, $args) = @_;
    my $scope = $self->_new_scope(undef);
    $scope->{varargs} = $args || [];
    eval { $self->_exec_block($scope, $chunk->{body}); };
    my $err = $@;
    if ($err) {
        return @{ $err->{values} } if ref($err) eq 'PureLua::Signal::Return';
        die $err;
    }
    return ();
}

# --- statements ------------------------------------------------------------

sub _exec_block {
    my ($self, $scope, $stmts) = @_;
    $self->_exec_stmt($scope, $_) for @$stmts;
}

sub _exec_stmt {
    my ($self, $scope, $stmt) = @_;
    my $type = $stmt->{type};

    if ($type eq 'local') {
        my @vals = $self->_eval_explist($scope, $stmt->{exprs});
        my $names = $stmt->{names};
        $self->_declare_local($scope, $names->[$_], $vals[$_]) for 0 .. $#$names;
        return;
    }
    if ($type eq 'localfunction') {
        # Desugars to `local f; f = function() ... end`, not `local f =
        # function()...end` - the cell must exist before the closure is
        # built so the function body can call itself recursively.
        $self->_declare_local($scope, $stmt->{name}, undef);
        my $cell = $self->_lookup_cell($scope, $stmt->{name});
        $$cell = $self->_make_function($scope, $stmt->{body});
        return;
    }
    if ($type eq 'assign') {
        $self->_exec_assign($scope, $stmt->{targets}, $stmt->{exprs});
        return;
    }
    if ($type eq 'callstat') {
        $self->_eval_call_multi($scope, $stmt->{call});
        return;
    }
    if ($type eq 'do') {
        $self->_exec_block($self->_new_scope($scope), $stmt->{body});
        return;
    }
    if ($type eq 'if') {
        for my $clause (@{ $stmt->{clauses} }) {
            if (_is_truthy($self->_eval_expr($scope, $clause->{cond}))) {
                $self->_exec_block($self->_new_scope($scope), $clause->{body});
                return;
            }
        }
        $self->_exec_block($self->_new_scope($scope), $stmt->{else_body}) if $stmt->{else_body};
        return;
    }
    if ($type eq 'while') {
        while (_is_truthy($self->_eval_expr($scope, $stmt->{cond}))) {
            last unless $self->_run_loop_body($self->_new_scope($scope), $stmt->{body});
        }
        return;
    }
    if ($type eq 'repeat') {
        while (1) {
            my $iter_scope = $self->_new_scope($scope);
            my $broke = 0;
            eval { $self->_exec_block($iter_scope, $stmt->{body}); };
            if (my $err = $@) {
                die $err unless ref($err) eq 'PureLua::Signal::Break';
                $broke = 1;
            }
            last if $broke;
            last if _is_truthy($self->_eval_expr($iter_scope, $stmt->{cond}));
        }
        return;
    }
    if ($type eq 'fornum') {
        $self->_exec_fornum($scope, $stmt);
        return;
    }
    if ($type eq 'forin') {
        $self->_exec_forin($scope, $stmt);
        return;
    }
    if ($type eq 'function') {
        $self->_assign_to($scope, $stmt->{target}, $self->_make_function($scope, $stmt->{body}));
        return;
    }
    if ($type eq 'return') {
        die PureLua::Signal::Return->new([ $self->_eval_explist($scope, $stmt->{exprs}) ]);
    }
    if ($type eq 'break') {
        die PureLua::Signal::Break->new;
    }
    $self->_runtime_error("unknown statement type '$type'", $stmt->{line});
}

# Runs a loop iteration's block in the given (already-fresh) scope;
# returns 1 to keep looping, 0 if the body broke out.
sub _run_loop_body {
    my ($self, $scope, $body) = @_;
    eval { $self->_exec_block($scope, $body); };
    if (my $err = $@) {
        die $err unless ref($err) eq 'PureLua::Signal::Break';
        return 0;
    }
    return 1;
}

sub _exec_fornum {
    my ($self, $scope, $stmt) = @_;
    my $line  = $stmt->{line};
    my $start = $self->_tonumber_strict($self->_eval_expr($scope, $stmt->{start}), $line);
    my $stop  = $self->_tonumber_strict($self->_eval_expr($scope, $stmt->{stop}), $line);
    my $step  = defined $stmt->{step}
        ? $self->_tonumber_strict($self->_eval_expr($scope, $stmt->{step}), $line)
        : 1;
    $self->_runtime_error("'for' step is zero", $line) if $step == 0;

    my $val = $start;
    while ($step > 0 ? $val <= $stop : $val >= $stop) {
        my $iter_scope = $self->_new_scope($scope);
        $self->_declare_local($iter_scope, $stmt->{var}, $val);
        last unless $self->_run_loop_body($iter_scope, $stmt->{body});
        $val += $step;
    }
}

sub _exec_forin {
    my ($self, $scope, $stmt) = @_;
    my ($iter_func, $state, $ctrl) = $self->_eval_explist($scope, $stmt->{exprs});

    while (1) {
        my @results = $self->_call_value($iter_func, [ $state, $ctrl ], $stmt->{line});
        last unless defined $results[0];
        $ctrl = $results[0];

        my $iter_scope = $self->_new_scope($scope);
        my $names = $stmt->{names};
        $self->_declare_local($iter_scope, $names->[$_], $results[$_]) for 0 .. $#$names;
        last unless $self->_run_loop_body($iter_scope, $stmt->{body});
    }
}

sub _exec_assign {
    my ($self, $scope, $targets, $exprs) = @_;
    # Evaluate every target's object/key before evaluating the RHS - real
    # Lua's exact cross-side ordering is unspecified, but reading index
    # expressions before any write happens is the least surprising choice.
    my @prepared;
    for my $t (@$targets) {
        if ($t->{type} eq 'name') {
            push @prepared, { name => $t->{name} };
        }
        else {
            push @prepared, {
                obj => $self->_eval_expr($scope, $t->{obj}),
                key => $self->_eval_expr($scope, $t->{key}),
            };
        }
    }
    my @vals = $self->_eval_explist($scope, $exprs);
    for my $i (0 .. $#prepared) {
        my $p = $prepared[$i];
        if (exists $p->{name}) { $self->_set_var($scope, $p->{name}, $vals[$i]); }
        else                   { $self->_table_set($p->{obj}, $p->{key}, $vals[$i]); }
    }
}

sub _assign_to {
    my ($self, $scope, $target, $value) = @_;
    if ($target->{type} eq 'name') {
        $self->_set_var($scope, $target->{name}, $value);
    }
    else {
        my $obj = $self->_eval_expr($scope, $target->{obj});
        my $key = $self->_eval_expr($scope, $target->{key});
        $self->_table_set($obj, $key, $value);
    }
}

# --- expressions ------------------------------------------------------------
#
# `_eval_expr` always yields exactly one value (Lua's "adjust to 1"
# rule). `_eval_multi`/`_eval_explist` implement the one place that
# rule doesn't apply: the *last* expression in a list expands to all of
# its results if it's a call or `...`.

sub _eval_explist {
    my ($self, $scope, $exprs) = @_;
    return () unless @$exprs;
    my @out = map { $self->_eval_expr($scope, $exprs->[$_]) } 0 .. $#$exprs - 1;
    push @out, $self->_eval_multi($scope, $exprs->[-1]);
    return @out;
}

sub _eval_multi {
    my ($self, $scope, $node) = @_;
    my $type = $node->{type};
    return $self->_eval_call_multi($scope, $node)       if $type eq 'call';
    return $self->_eval_methodcall_multi($scope, $node) if $type eq 'methodcall';
    return @{ $self->_get_varargs($scope) }              if $type eq 'vararg';
    return ( $self->_eval_expr($scope, $node) );
}

sub _eval_call_multi {
    my ($self, $scope, $node) = @_;
    my $func = $self->_eval_expr($scope, $node->{func});
    my @args = $self->_eval_explist($scope, $node->{args});
    return $self->_call_value($func, \@args, $node->{line});
}

sub _eval_methodcall_multi {
    my ($self, $scope, $node) = @_;
    my $obj = $self->_eval_expr($scope, $node->{obj});

    # Real Lua gives strings a built-in metatable routing `s:method(...)`
    # through the `string` library - the one hard-wired metatable this
    # engine supports, not a general metatable mechanism.
    my $func;
    if (ref($obj) eq 'PureLua::Str') {
        my $strlib = $self->{globals}->get(PureLua::Table::wrap_str('string'));
        $func = $strlib->get(PureLua::Table::wrap_str($node->{method})) if $strlib;
        $self->_runtime_error("attempt to call method '$node->{method}' (no string library installed)", $node->{line})
            unless defined $func;
    }
    else {
        $func = $self->_table_get($obj, PureLua::Table::wrap_str($node->{method}), $node->{line});
    }
    my @args = $self->_eval_explist($scope, $node->{args});
    return $self->_call_value($func, [ $obj, @args ], $node->{line});
}

sub _call_value {
    my ($self, $func, $args, $line) = @_;
    return $func->(@$args) if ref($func) eq 'CODE';
    return $self->_call_lua_function($func, $args) if ref($func) eq 'PureLua::Function';
    $self->_runtime_error('attempt to call a non-function value', $line);
}

sub _call_lua_function {
    my ($self, $func, $args) = @_;
    my $scope  = $self->_new_scope($func->{closure_env});
    my $params = $func->{params};
    $self->_declare_local($scope, $params->[$_], $args->[$_]) for 0 .. $#$params;
    if ($func->{vararg}) {
        my @extra = @$args[ scalar(@$params) .. $#$args ];
        $scope->{varargs} = \@extra;
    }
    eval { $self->_exec_block($scope, $func->{body}); };
    my $err = $@;
    if ($err) {
        return @{ $err->{values} } if ref($err) eq 'PureLua::Signal::Return';
        die $err;
    }
    return ();
}

sub _make_function {
    my ($self, $scope, $funcbody) = @_;
    return bless {
        params      => $funcbody->{params},
        vararg      => $funcbody->{vararg},
        body        => $funcbody->{body},
        closure_env => $scope,
    }, 'PureLua::Function';
}

sub _eval_expr {
    my ($self, $scope, $node) = @_;
    my $type = $node->{type};

    return undef                                   if $type eq 'nil';
    return $TRUE                                    if $type eq 'true';
    return $FALSE                                    if $type eq 'false';
    return $node->{value}                            if $type eq 'number';
    return _mkstr($node->{value})                    if $type eq 'string';
    return $self->_get_var($scope, $node->{name})    if $type eq 'name';
    return $self->_eval_expr($scope, $node->{expr})  if $type eq 'paren';
    return $self->_get_varargs($scope)->[0]          if $type eq 'vararg';
    return $self->_make_function($scope, $node->{body}) if $type eq 'function';
    return $self->_eval_table($scope, $node)         if $type eq 'table';
    return $self->_eval_binop($scope, $node)         if $type eq 'binop';
    return $self->_eval_unop($scope, $node)          if $type eq 'unop';

    if ($type eq 'index') {
        my $obj = $self->_eval_expr($scope, $node->{obj});
        my $key = $self->_eval_expr($scope, $node->{key});
        return $self->_table_get($obj, $key, $node->{line});
    }
    if ($type eq 'call') {
        my @r = $self->_eval_call_multi($scope, $node);
        return $r[0];
    }
    if ($type eq 'methodcall') {
        my @r = $self->_eval_methodcall_multi($scope, $node);
        return $r[0];
    }
    $self->_runtime_error("unknown expression type '$type'", $node->{line});
}

sub _eval_table {
    my ($self, $scope, $node) = @_;
    my $tbl    = PureLua::Table->new;
    my $fields = $node->{fields};
    my $next_index = 1;

    for my $i (0 .. $#$fields) {
        my $f = $fields->[$i];
        if ($f->{type} eq 'named') {
            $tbl->set(_mkstr($f->{key}), $self->_eval_expr($scope, $f->{value}));
        }
        elsif ($f->{type} eq 'keyed') {
            $tbl->set($self->_eval_expr($scope, $f->{key}), $self->_eval_expr($scope, $f->{value}));
        }
        elsif ($i == $#$fields) {
            # Only the last positional field expands to multiple slots.
            $tbl->set($next_index++, $_) for $self->_eval_multi($scope, $f->{value});
        }
        else {
            $tbl->set($next_index++, $self->_eval_expr($scope, $f->{value}));
        }
    }
    return $tbl;
}

sub _table_get {
    my ($self, $obj, $key, $line) = @_;
    return $obj->get($key) if ref($obj) eq 'PureLua::Table';
    $self->_runtime_error('attempt to index a ' . $self->_typename($obj) . ' value', $line);
}

sub _table_set {
    my ($self, $obj, $key, $value, $line) = @_;
    if (ref($obj) eq 'PureLua::Table') { $obj->set($key, $value); return; }
    $self->_runtime_error('attempt to index a ' . $self->_typename($obj) . ' value', $line);
}

sub _typename {
    my ($self, $v) = @_;
    return 'nil' unless defined $v;
    return 'boolean'  if ref($v) eq 'PureLua::Boolean';
    return 'table'    if ref($v) eq 'PureLua::Table';
    return 'function' if ref($v) eq 'PureLua::Function' || ref($v) eq 'CODE';
    return 'string'   if ref($v) eq 'PureLua::Str';
    return 'number'; # unblessed and unreffed - with strings wrapped, nothing else is left
}

# Arithmetic coercion: a real Lua number passes straight through; a Lua
# string coerces only if its text looks numeric (Lua's own rule, e.g.
# "5"+2 == 7); anything else (nil/boolean/table/function) isn't coercible.
sub _tonumber {
    my ($self, $v) = @_;
    return undef unless defined $v;
    return $v + 0 unless ref $v;
    return undef unless ref($v) eq 'PureLua::Str';
    my $s = $$v;
    # Perl's own string-to-number coercion (`+0`) does NOT understand a
    # "0x..." prefix (`"0x10" + 0` is 0, not 16) - found via the wider
    # corpus sweep tripping over a literal `"0x2010"` string value real
    # Lua auto-coerces correctly. The hex branch needs `hex()` explicitly.
    if ($s =~ /\A\s*(-?)0[xX]([0-9a-fA-F]+)\s*\z/) {
        my $n = hex($2);
        return $1 eq '-' ? -$n : $n;
    }
    return $s + 0 if $s =~ /\A\s*-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?\s*\z/;
    return undef;
}

sub _tonumber_strict {
    my ($self, $v, $line) = @_;
    my $n = $self->_tonumber($v);
    $self->_runtime_error('attempt to perform arithmetic on a ' . $self->_typename($v) . ' value', $line)
        unless defined $n;
    return $n;
}

# Returns a PLAIN Perl string - the text itself, not a Lua value. Used
# both internally (error messages) and as the building block for
# anything that DOES need to hand a real Lua string back (string.format,
# `..`, tostring()), which wrap this return value themselves. Critically,
# an actual Lua string passes through verbatim, never reformatted - that
# was the bug: re-running an already-correct "00" through a number
# formatter silently dropped its leading zero.
sub _tostring {
    my ($self, $v) = @_;
    return 'nil' unless defined $v;
    return $$v ? 'true' : 'false' if ref($v) eq 'PureLua::Boolean';
    return "table: $v"            if ref($v) eq 'PureLua::Table';
    return "function: $v"         if ref($v) eq 'PureLua::Function' || ref($v) eq 'CODE';
    return $$v                    if ref($v) eq 'PureLua::Str';
    return sprintf('%.14g', $v); # must be a number
}

sub _lua_floor {
    my ($self, $x) = @_;
    my $i = int($x);
    return ($i > $x) ? $i - 1 : $i;
}

sub _values_equal {
    my ($self, $l, $r) = @_;
    my ($tl, $tr) = ($self->_typename($l), $self->_typename($r));
    return 0 if $tl ne $tr;
    return 1 if $tl eq 'nil';
    return $l == $r if $tl eq 'number';
    return $$l eq $$r if $tl eq 'string';
    return (!!$$l) == (!!$$r) if $tl eq 'boolean';
    return $l == $r; # table/function: reference identity
}

sub _compare {
    my ($self, $op, $l, $r, $line) = @_;
    my ($tl, $tr) = ($self->_typename($l), $self->_typename($r));
    $self->_runtime_error("attempt to compare $tl with $tr", $line) if $tl ne $tr;
    $self->_runtime_error("attempt to compare two $tl values", $line)
        unless $tl eq 'number' || $tl eq 'string';

    my ($lv, $rv) = $tl eq 'string' ? ( $$l, $$r ) : ( $l, $r );
    my $result =
        $op eq '<'  ? ($tl eq 'number' ? $lv < $rv  : $lv lt $rv) :
        $op eq '<=' ? ($tl eq 'number' ? $lv <= $rv : $lv le $rv) :
        $op eq '>'  ? ($tl eq 'number' ? $lv > $rv  : $lv gt $rv) :
                      ($tl eq 'number' ? $lv >= $rv : $lv ge $rv);
    return _bool($result);
}

sub _eval_binop {
    my ($self, $scope, $node) = @_;
    my $op = $node->{op};

    if ($op eq 'and') {
        my $l = $self->_eval_expr($scope, $node->{lhs});
        return $l unless _is_truthy($l);
        return $self->_eval_expr($scope, $node->{rhs});
    }
    if ($op eq 'or') {
        my $l = $self->_eval_expr($scope, $node->{lhs});
        return $l if _is_truthy($l);
        return $self->_eval_expr($scope, $node->{rhs});
    }

    my $l    = $self->_eval_expr($scope, $node->{lhs});
    my $r    = $self->_eval_expr($scope, $node->{rhs});
    my $line = $node->{line};

    return $self->_tonumber_strict($l, $line) + $self->_tonumber_strict($r, $line) if $op eq '+';
    return $self->_tonumber_strict($l, $line) - $self->_tonumber_strict($r, $line) if $op eq '-';
    return $self->_tonumber_strict($l, $line) * $self->_tonumber_strict($r, $line) if $op eq '*';
    return $self->_tonumber_strict($l, $line) / $self->_tonumber_strict($r, $line) if $op eq '/';
    return $self->_tonumber_strict($l, $line) ** $self->_tonumber_strict($r, $line) if $op eq '^';
    if ($op eq '%') {
        my ($a, $b) = ($self->_tonumber_strict($l, $line), $self->_tonumber_strict($r, $line));
        return $a - $self->_lua_floor($a / $b) * $b;
    }
    return _mkstr($self->_tostring($l) . $self->_tostring($r)) if $op eq '..';

    return _bool($self->_values_equal($l, $r))  if $op eq '==';
    return _bool(!$self->_values_equal($l, $r)) if $op eq '~=';
    return $self->_compare($op, $l, $r, $line)  if $op eq '<' || $op eq '<=' || $op eq '>' || $op eq '>=';

    $self->_runtime_error("unknown binary operator '$op'", $line);
}

sub _eval_unop {
    my ($self, $scope, $node) = @_;
    my $v  = $self->_eval_expr($scope, $node->{expr});
    my $op = $node->{op};

    return -$self->_tonumber_strict($v, $node->{line}) if $op eq '-';
    return _is_truthy($v) ? $FALSE : $TRUE              if $op eq 'not';
    if ($op eq '#') {
        return $v->lua_length if ref($v) eq 'PureLua::Table';
        return length($$v) if ref($v) eq 'PureLua::Str';
        $self->_runtime_error('attempt to get length of a ' . $self->_typename($v) . ' value', $node->{line});
    }
    $self->_runtime_error("unknown unary operator '$op'", $node->{line});
}

1;

#!/usr/bin/env perl
#
# ac-ng.pl — LAN control driven directly by the Midea-cloud-issued,
# device-model-specific protocol script (lua/<model>.lua, from
# ac-check.pl --lua), run through lua/PureLua, a pure-Perl Lua 5.1
# interpreter — instead of a hand-ported byte-offset table or a generated
# protocol map. No CPAN modules, no lua/luajit at runtime.
#
# Network/crypto layer (packet framing, AES-128-ECB, KEY_SIGN) is the same
# approach as ac.pl, duplicated here per this project's convention of
# self-contained scripts.
#
use strict;
use warnings;

use Getopt::Long ();
use POSIX        ();
use List::Util   ();
use Socket       ();
use IO::Socket   ();
use IO::Handle   ();
# PureLua::* is bundled directly above — no external lookup needed.

use constant {
    PORT         => 6444,
    TIMEOUT      => 4,
    RETRY        => 8,
    BLOCK_LEN    => 16,
    RESPONSE_LEN => 256,
    EMPTY_STR    => '',
};

use constant { KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S' };
use constant { KEY => '6a92ef406bad2f0359baad994171ea6d' }; # md5(KEY_SIGN)

use constant {
    PACKET => [
        0x5a, 0x5a, 0x01, 0x11, 0x68, 0x00, 0x20, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ],
    # Generic status query (type 0x41) — not property-specific, same for
    # every model, so this is not part of the generated map.
    STATUS_QUERY => [
        0xaa, 0x20, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x41, 0x81, 0x00, 0xff, 0x03, 0xff,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ],
};

our $DEBUG = 0;

sub dbg {
    return unless $DEBUG;
    my ( $label, $data ) = @_;
    printf STDERR "[DEBUG] %-10s %s\n", $label,
      ref($data) eq 'ARRAY' ? ahex($data) : $data;
}

sub ahex {
    my ($item) = @_;
    return ref($item) eq 'ARRAY'
      ? join( EMPTY_STR, map { sprintf( '%.2x', $_ ) } @{$item} )
      : unpack( 'H*', $item );
}

# --- Pure Perl AES-128-ECB (verbatim from ac.pl) ---
{
    package PureAES;
    use strict;
    use warnings;

    my @S = (
        0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
        0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
        0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
        0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
        0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
        0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
        0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
        0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
        0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
        0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
        0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
        0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
        0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
        0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
        0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
        0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16,
    );
    my @Si = (
        0x52,0x09,0x6a,0xd5,0x30,0x36,0xa5,0x38,0xbf,0x40,0xa3,0x9e,0x81,0xf3,0xd7,0xfb,
        0x7c,0xe3,0x39,0x82,0x9b,0x2f,0xff,0x87,0x34,0x8e,0x43,0x44,0xc4,0xde,0xe9,0xcb,
        0x54,0x7b,0x94,0x32,0xa6,0xc2,0x23,0x3d,0xee,0x4c,0x95,0x0b,0x42,0xfa,0xc3,0x4e,
        0x08,0x2e,0xa1,0x66,0x28,0xd9,0x24,0xb2,0x76,0x5b,0xa2,0x49,0x6d,0x8b,0xd1,0x25,
        0x72,0xf8,0xf6,0x64,0x86,0x68,0x98,0x16,0xd4,0xa4,0x5c,0xcc,0x5d,0x65,0xb6,0x92,
        0x6c,0x70,0x48,0x50,0xfd,0xed,0xb9,0xda,0x5e,0x15,0x46,0x57,0xa7,0x8d,0x9d,0x84,
        0x90,0xd8,0xab,0x00,0x8c,0xbc,0xd3,0x0a,0xf7,0xe4,0x58,0x05,0xb8,0xb3,0x45,0x06,
        0xd0,0x2c,0x1e,0x8f,0xca,0x3f,0x0f,0x02,0xc1,0xaf,0xbd,0x03,0x01,0x13,0x8a,0x6b,
        0x3a,0x91,0x11,0x41,0x4f,0x67,0xdc,0xea,0x97,0xf2,0xcf,0xce,0xf0,0xb4,0xe6,0x73,
        0x96,0xac,0x74,0x22,0xe7,0xad,0x35,0x85,0xe2,0xf9,0x37,0xe8,0x1c,0x75,0xdf,0x6e,
        0x47,0xf1,0x1a,0x71,0x1d,0x29,0xc5,0x89,0x6f,0xb7,0x62,0x0e,0xaa,0x18,0xbe,0x1b,
        0xfc,0x56,0x3e,0x4b,0xc6,0xd2,0x79,0x20,0x9a,0xdb,0xc0,0xfe,0x78,0xcd,0x5a,0xf4,
        0x1f,0xdd,0xa8,0x33,0x88,0x07,0xc7,0x31,0xb1,0x12,0x10,0x59,0x27,0x80,0xec,0x5f,
        0x60,0x51,0x7f,0xa9,0x19,0xb5,0x4a,0x0d,0x2d,0xe5,0x7a,0x9f,0x93,0xc9,0x9c,0xef,
        0xa0,0xe0,0x3b,0x4d,0xae,0x2a,0xf5,0xb0,0xc8,0xeb,0xbb,0x3c,0x83,0x53,0x99,0x61,
        0x17,0x2b,0x04,0x7e,0xba,0x77,0xd6,0x26,0xe1,0x69,0x14,0x63,0x55,0x21,0x0c,0x7d,
    );
    my @RCON = ( 0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1b,0x36 );

    sub _mul {
        my ( $a, $b ) = @_;
        my $r = 0;
        for ( 1 .. 8 ) {
            $r ^= $a if $b & 1;
            my $h = $a & 0x80;
            $a = ( $a << 1 ) & 0xff;
            $a ^= 0x1b if $h;
            $b >>= 1;
        }
        return $r;
    }

    sub _key_expand {
        my @k = unpack( 'C*', $_[0] );
        my @w;
        $w[$_] = ( $k[$_*4] << 24 ) | ( $k[$_*4+1] << 16 ) | ( $k[$_*4+2] << 8 ) | $k[$_*4+3]
            for 0 .. 3;
        for my $i ( 4 .. 43 ) {
            my $t = $w[$i-1];
            if ( $i % 4 == 0 ) {
                $t = ( ( $t << 8 ) | ( $t >> 24 ) ) & 0xffffffff;
                $t = ( $S[($t>>24)&0xff] << 24 ) | ( $S[($t>>16)&0xff] << 16 )
                   | ( $S[($t>> 8)&0xff] <<  8 ) |   $S[ $t      &0xff];
                $t ^= $RCON[$i/4-1] << 24;
            }
            $w[$i] = ( $w[$i-4] ^ $t ) & 0xffffffff;
        }
        return \@w;
    }

    sub _add_rk {
        my ( $s, $w, $rnd ) = @_;
        for my $c ( 0 .. 3 ) {
            my $wrd = $w->[$rnd*4+$c];
            $s->[$c*4]   ^= ( $wrd >> 24 ) & 0xff;
            $s->[$c*4+1] ^= ( $wrd >> 16 ) & 0xff;
            $s->[$c*4+2] ^= ( $wrd >>  8 ) & 0xff;
            $s->[$c*4+3] ^=   $wrd         & 0xff;
        }
    }

    sub _enc_block {
        my ( $in, $w ) = @_;
        my @s = @$in;
        _add_rk( \@s, $w, 0 );
        for my $rnd ( 1 .. 9 ) {
            $s[$_] = $S[$s[$_]] for 0 .. 15;
            @s[1,5,9,13]  = @s[5,9,13,1];
            @s[2,6,10,14] = @s[10,14,2,6];
            @s[3,7,11,15] = @s[15,3,7,11];
            for my $c ( 0 .. 3 ) {
                my ( $a, $b, $cc, $d ) = @s[$c*4 .. $c*4+3];
                $s[$c*4]   = _mul(2,$a) ^ _mul(3,$b) ^       $cc  ^       $d;
                $s[$c*4+1] =      $a   ^ _mul(2,$b)  ^ _mul(3,$cc) ^       $d;
                $s[$c*4+2] =      $a   ^       $b    ^ _mul(2,$cc) ^ _mul(3,$d);
                $s[$c*4+3] = _mul(3,$a) ^      $b   ^       $cc   ^ _mul(2,$d);
            }
            _add_rk( \@s, $w, $rnd );
        }
        $s[$_] = $S[$s[$_]] for 0 .. 15;
        @s[1,5,9,13]  = @s[5,9,13,1];
        @s[2,6,10,14] = @s[10,14,2,6];
        @s[3,7,11,15] = @s[15,3,7,11];
        _add_rk( \@s, $w, 10 );
        return \@s;
    }

    sub _dec_block {
        my ( $in, $w ) = @_;
        my @s = @$in;
        _add_rk( \@s, $w, 10 );
        for my $rnd ( reverse 1 .. 9 ) {
            @s[1,5,9,13]  = @s[13,1,5,9];
            @s[2,6,10,14] = @s[10,14,2,6];
            @s[3,7,11,15] = @s[7,11,15,3];
            $s[$_] = $Si[$s[$_]] for 0 .. 15;
            _add_rk( \@s, $w, $rnd );
            for my $c ( 0 .. 3 ) {
                my ( $a, $b, $cc, $d ) = @s[$c*4 .. $c*4+3];
                $s[$c*4]   = _mul(14,$a) ^ _mul(11,$b) ^ _mul(13,$cc) ^ _mul( 9,$d);
                $s[$c*4+1] = _mul( 9,$a) ^ _mul(14,$b) ^ _mul(11,$cc) ^ _mul(13,$d);
                $s[$c*4+2] = _mul(13,$a) ^ _mul( 9,$b) ^ _mul(14,$cc) ^ _mul(11,$d);
                $s[$c*4+3] = _mul(11,$a) ^ _mul(13,$b) ^ _mul( 9,$cc) ^ _mul(14,$d);
            }
        }
        @s[1,5,9,13]  = @s[13,1,5,9];
        @s[2,6,10,14] = @s[10,14,2,6];
        @s[3,7,11,15] = @s[7,11,15,3];
        $s[$_] = $Si[$s[$_]] for 0 .. 15;
        _add_rk( \@s, $w, 0 );
        return \@s;
    }

    sub encrypt_ecb {
        my ( $data, $key ) = @_;
        my @b = unpack( 'C*', $data );
        my $w = _key_expand($key);
        my $out = '';
        $out .= pack( 'C*', @{ _enc_block( [@b[$_*16 .. $_*16+15]], $w ) } ) for 0 .. $#b / 16;
        return $out;
    }

    sub decrypt_ecb {
        my ( $data, $key ) = @_;
        my @b = unpack( 'C*', $data );
        my $w = _key_expand($key);
        my $out = '';
        $out .= pack( 'C*', @{ _dec_block( [@b[$_*16 .. $_*16+15]], $w ) } ) for 0 .. $#b / 16;
        return $out;
    }

    1;
}

sub inflate { return [ map { ord } split //, $_[0] ] }
sub deflate { return join EMPTY_STR, map { defined($_) ? chr($_) : chr(0) } @{ $_[0] } }
sub encrypt { return inflate( PureAES::encrypt_ecb( deflate( $_[0] ), pack( 'H*', KEY ) ) ) }
sub decrypt { return inflate( PureAES::decrypt_ecb( deflate( $_[0] ), pack( 'H*', KEY ) ) ) }

sub md5 {
    my ($msg) = $_[0];
    my ( $a, $b, $c, $d ) = ( 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 );
    my @T = map { int( 4294967296 * abs( sin( $_ + 1 ) ) ) } 0 .. 63;
    my @S = ( (7,12,17,22) x 4, (5,9,14,20) x 4, (4,11,16,23) x 4, (6,10,15,21) x 4 );
    my $msg_len = length($msg);
    $msg .= pack( 'C', 0x80 );
    $msg .= pack( 'C', 0 ) while ( length($msg) % 64 != 56 );
    $msg .= pack( 'V2', $msg_len * 8, 0 );
    my $rotl = sub { my ( $x, $n ) = @_; $x &= 0xffffffff; ( ( $x << $n ) | ( $x >> ( 32 - $n ) ) ) & 0xffffffff };
    for ( my $i = 0; $i < length($msg); $i += 64 ) {
        my @M = unpack( 'V16', substr( $msg, $i, 64 ) );
        my ( $A, $B, $C, $D ) = ( $a, $b, $c, $d );
        for my $j ( 0 .. 63 ) {
            my ( $F, $g );
            if ( $j < 16 ) { $F = ( $B & $C ) | ( ( ~$B & 0xffffffff ) & $D ); $g = $j; }
            elsif ( $j < 32 ) { $F = ( $D & $B ) | ( ( ~$D & 0xffffffff ) & $C ); $g = ( 5 * $j + 1 ) % 16; }
            elsif ( $j < 48 ) { $F = $B ^ $C ^ $D; $g = ( 3 * $j + 5 ) % 16; }
            else { $F = $C ^ ( $B | ( ~$D & 0xffffffff ) ); $g = ( 7 * $j ) % 16; }
            $F = ( $F + $A + $T[$j] + $M[$g] ) & 0xffffffff;
            $A = $D; $D = $C; $C = $B; $B = ( $B + $rotl->( $F, $S[$j] ) ) & 0xffffffff;
        }
        $a = ( $a + $A ) & 0xffffffff; $b = ( $b + $B ) & 0xffffffff;
        $c = ( $c + $C ) & 0xffffffff; $d = ( $d + $D ) & 0xffffffff;
    }
    return pack( 'V4', $a, $b, $c, $d );
}

use constant {
    CRC8_TABLE_GEN => sub {
        my ( $p, $v ) = ( 0x00, $_[1] );
        do { $p |= 0x01 << ( $_[0] - $_ ) if $v & 0x01; $v = $v >> 0x01; }
          for 0x01 .. $_[0];
        return map {
            my $i = $_;
            $i = ( $i >> 0x01 ) ^ ( $i & 0x01 && $p ) for 0x00 .. 0x07;
            $i & 0x02**$_[0] - 0x01
        } 0x00 .. 0xff;
    }
};
use constant { CRC8_TABLE => [ CRC8_TABLE_GEN->( 8, 0x0131 ) ] };

sub crc8 {
    my $crc = 0;
    $crc = CRC8_TABLE->[ $crc ^ $_ ] for @{ $_[0] };
    return $crc;
}

# Pads, encrypts and outer-wraps a command that already ends in its own
# trailing checksum — used for a frame built by the device .lua script's
# own jsonToData/getTotalMsg, which computes that checksum itself
# (confirmed identical algorithm to the one `packet()` below adds: both
# are `(~sum(bytes[1..end]) + 1) & 0xff`). Calling `packet()` on such a
# frame would add a second, wrong checksum on top of the real one.
sub wrap_packet {
    my ($command) = @_;
    my $packet = [ @{ +PACKET } ];

    my $pad = BLOCK_LEN - ( scalar(@$command) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt($command) };
    $packet->[0x04] = scalar(@$packet) + BLOCK_LEN;
    push @{$packet}, @{ inflate md5( deflate($packet) . KEY_SIGN ) };

    return $packet;
}

sub packet {
    my ($command) = @_;
    push @{$command}, ( ( ~List::Util::sum0( @{$command}[ 0x01 .. $#$command ] ) + 0x01 ) & 0xff );
    return wrap_packet($command);
}

sub net_request {
    my ( $device_ip, $data, $recv_len ) = @_;
    $recv_len //= RESPONSE_LEN;

    dbg( '>> TX', $data );

    my $client = IO::Socket->new(
        Domain    => IO::Socket::AF_INET,
        Type      => IO::Socket::SOCK_STREAM,
        Proto     => 'tcp',
        Timeout   => TIMEOUT,
        ReusePort => 1,
        PeerPort  => PORT,
        PeerHost  => $device_ip,
    ) or die "Socket error: $@";

    $client->send( deflate $data ) == scalar @{$data}
      or $client->close(), die "Send error";

    my $buffer = '';
    {
        local $SIG{ALRM} = sub { die "timeout" };
        alarm TIMEOUT;
        eval { $client->recv( $buffer, $recv_len ); 1 } or do {
            alarm 0;
            $client->close();
            die $@ || "timeout";
        };
        alarm 0;
    }
    $client->close();

    my $len = length($buffer);
    die "no response" unless $len;

    my $raw = inflate $buffer;
    dbg( '<< RX', $raw );

    my $enc_end = $len >= 17 ? $len - 17 : 0x27;
    $enc_end = 0x27 if $enc_end < 0x28;
    return [ @{$raw}[ 0x28 .. $enc_end ] ];
}

sub send_request {
    my ( $device_ip, $data, $recv_len ) = @_;
    my $response = decrypt( net_request( $device_ip, $data, $recv_len ) );
    my $unpadded = [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
    dbg( '<< RX/cmd', $unpadded );
    return $unpadded;
}

sub request_with_retry {
    my ( $device_ip, $packet, $validate ) = @_;
    my $last_error;
    my $retry = RETRY;
    while ( --$retry ) {
        my $data = eval {
            my $d = send_request( $device_ip, $packet );
            die "unexpected response type\n" if $validate && !$validate->($d);
            $d;
        };
        if ( !$data ) { $last_error = $@; next; }
        return $data;
    }
    die $last_error;
}

# --- Minimal JSON decoder (reads the generated map only; no encoder needed) ---

sub json_decode {
    my ($text) = @_;
    my $pos = 0;
    my $parse_value;

    my $skip_ws = sub { $pos++ while substr( $text, $pos, 1 ) =~ /^\s$/ };

    my $parse_string = sub {
        $pos++; # opening quote
        my $out = '';
        while (1) {
            my $c = substr( $text, $pos, 1 );
            die "unterminated string" if $c eq '';
            last if $c eq '"';
            if ( $c eq '\\' ) {
                my $e = substr( $text, $pos + 1, 1 );
                my %map = ( n => "\n", r => "\r", t => "\t", '"' => '"', '\\' => '\\', '/' => '/' );
                if ( exists $map{$e} ) { $out .= $map{$e}; $pos += 2; }
                elsif ( $e eq 'u' ) { $out .= chr( hex( substr( $text, $pos + 2, 4 ) ) % 256 ); $pos += 6; }
                else { $out .= $e; $pos += 2; }
            }
            else { $out .= $c; $pos++; }
        }
        $pos++; # closing quote
        return $out;
    };

    $parse_value = sub {
        $skip_ws->();
        my $c = substr( $text, $pos, 1 );
        if ( $c eq '"' ) { return $parse_string->(); }
        if ( $c eq '{' ) {
            $pos++; $skip_ws->();
            my %obj;
            if ( substr( $text, $pos, 1 ) eq '}' ) { $pos++; return \%obj; }
            while (1) {
                $skip_ws->();
                my $k = $parse_string->();
                $skip_ws->();
                $pos++; # colon
                $obj{$k} = $parse_value->();
                $skip_ws->();
                my $d = substr( $text, $pos, 1 );
                $pos++;
                last if $d eq '}';
                die "expected , or } " unless $d eq ',';
            }
            return \%obj;
        }
        if ( $c eq '[' ) {
            $pos++; $skip_ws->();
            my @arr;
            if ( substr( $text, $pos, 1 ) eq ']' ) { $pos++; return \@arr; }
            while (1) {
                push @arr, $parse_value->();
                $skip_ws->();
                my $d = substr( $text, $pos, 1 );
                $pos++;
                last if $d eq ']';
                die "expected , or ] " unless $d eq ',';
            }
            return \@arr;
        }
        if ( substr( $text, $pos, 4 ) eq 'true' )  { $pos += 4; return 1; }
        if ( substr( $text, $pos, 5 ) eq 'false' ) { $pos += 5; return 0; }
        if ( substr( $text, $pos, 4 ) eq 'null' )  { $pos += 4; return undef; }
        my ($num) = ( substr( $text, $pos ) =~ /^(-?\d+\.?\d*(?:[eE][+-]?\d+)?)/ );
        die "bad JSON at pos $pos" unless defined $num;
        $pos += length($num);
        return $num + 0;
    };

    return $parse_value->();
}

# --- Device .lua script engine: runs the real jsonToData/dataToJson ---

# Fixed sentinel identity for every call — not a real per-unit serial,
# just the same value this project's generator (harness.lua) and its
# luajit cross-check tests already use, confirmed not to change the frame
# bytes beyond this. Revisit if a future model's script branches on it.
use constant { DEVICE_INFO_LUA => '{ deviceSubType = 2, deviceSN = "00000000000000Q1B0" }' };

# Bootstrap baseline this model's jsonToModel needs populated on every
# control call to avoid a nil-arithmetic crash (see harness.lua's own
# BASE_CONTROL for the full rationale) — model-specific; a different
# device .lua may need a different baseline.
my %BASE_CONTROL = (
    power             => 'on',  mode          => 'cool', temperature  => 24,
    small_temperature => 0,     wind_speed    => 50,      wind_swing_ud => 'off',
    wind_swing_lr     => 'off', strong_wind   => 'off',   comfort_sleep => 'off',
    purifier          => 'off', eco           => 'off',   dry           => 'off',
    prevent_cold      => 'off', power_saving  => 'off',   ptc           => 'off',
    sleep_status      => 0,
);

# wind_speed's control-side range is a raw 1-102 value, not a name (see
# keyP["fanspeedValue"] = checkBoundary(..., 1, 102) in the device script)
# — same codes this project's old byte-offset table used.
my %WIND_SPEED_CODE = ( auto => 102, high => 80, medium => 60, low => 40, silent => 20 );

sub lua_literal {
    my ($v) = @_;
    return $v if $v =~ /^-?\d+(?:\.\d+)?$/;
    ( my $esc = $v ) =~ s/([\\"])/\\$1/g;
    return qq{"$esc"};
}

sub lua_table_literal {
    my (%h) = @_;
    return '{' . join( ', ', map { "$_ = " . lua_literal( $h{$_} ) } sort keys %h ) . '}';
}

# Runs device_src (the Midea-cloud-issued model .lua script) followed by
# driver_src (a `return ...` calling its jsonToData/dataToJson) in one
# fresh interpreter. A fresh run per call sidesteps the keyP state leak
# documented in harness.lua — this project's CLI only ever makes one such
# call per process anyway.
sub lua_run {
    my ( $device_src, $driver_src ) = @_;
    my $eval = PureLua::Eval->new;
    PureLua::Stdlib->install($eval);
    my $ast = PureLua::Parser->parse("$device_src\n$driver_src");
    my ($result) = $eval->run($ast);
    return $eval->str_value($result);
}

# Extended ("B0/B1") properties this model's own query builder knows how
# to ask for by name, and binToModel knows how to decode the TLV answer
# back into — both confirmed by grepping the device script itself
# (`queryType == "..."` in jsonToData's query branch; the matching
# `messageBytes[cursor+0] == <tag>` in binToModel's B1 decode), not
# guessed. `little_angel` and `gentle_wind_sense` are genuinely
# write-only on this model — no query tag exists for either — so they're
# left out rather than silently queried for nothing.
my @EXTENDED_QUERY_PROPS = qw[
    prevent_super_cool auto_prevent_straight_wind self_clean wind_straight
    wind_avoid yb_wind_avoid intelligent_wind no_wind_sense fa_no_wind_sense
    child_prevent_cold_wind cool_hot_sense security even_wind single_tuyere
    extreme_wind voice_control pre_cool_hot water_pump intelligent_control
    nobody_energy_save
];

# binToModel's B0/B1 decode loop walks a shared byte cursor through every
# tag in one response, advancing it per tag by whatever fixed width the
# *script* assumes for that specific tag (5 bytes for most, more for a
# few). If a tag's real on-wire width on this unit doesn't match that
# assumption, every tag *after* it in the same response reads from the
# wrong offset — confirmed live: querying 10 properties at once decoded
# the first few as plausible small numbers, then degraded into garbage
# (one property came back as the raw tag byte of the *next* one). A tag's
# own value is only reliably at the position the script expects when
# nothing before it in the same response could have been misaligned —
# i.e. when it's the *last* property asked for. So every property here is
# queried in a 2-item batch, always second, behind a single fixed, known-
# stable anchor ("self_clean": confirmed by querying it alone to decode
# to a sane 0, independent of anything else). Costs one round trip per
# property instead of a handful, but is the only arrangement confirmed
# not to silently corrupt a neighbor's value.
use constant EXTENDED_QUERY_ANCHOR => 'self_clean';

# A query body under ~9 bytes (meaning very few properties were asked
# for) trips a different, separate bug: binToModel's own early-exit guard
# is `if (#binData < 8) then return nil end`, but `#` on a table indexed
# from 0 (as every bodyBytes table here is) ignores index 0 and undercounts
# by one — confirmed identical on real luajit, not an engine quirk — so a
# single-property query (whose body is exactly 8 bytes) always measures as
# 7 and always bails out empty, decoding nothing at all. The anchor pairing
# above exists partly for this reason too: a 2-property body is long
# enough to clear this guard.

# --- Device I/O ---

sub get_status {
    my ($ip) = @_;
    my $data = [ @{ +STATUS_QUERY } ];
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    return request_with_retry(
        $ip, packet($data),
        sub { defined $_[0]->[0x0a] && ( $_[0]->[0x0a] == 0xc0 || $_[0]->[0x0a] == 0xc1 ) }
    );
}

# Builds and sends a B1 query via the device script's own jsonToData (the
# same "query" JSON shape it already supports), the same way cmd_set lets
# it pick 0x40 vs 0xb0 — no hand-rolled tag table here either. The
# returned frame already carries its own trailing checksum (getTotalMsg
# again), hence wrap_packet, not packet(). The device acks a B1 query
# with a B0/B1-typed frame, mirroring the B0 SET ack already confirmed on
# the wire. Queries one property at a time (paired behind
# EXTENDED_QUERY_ANCHOR — see the comments above @EXTENDED_QUERY_PROPS for
# why) and skips any that come back unset: dataToJson's own returned JSON
# never includes extended fields at all (confirmed live — it decodes a B1
# response's TLV tags into keyP, then wipes keyP back to nil as its own
# cleanup before returning, the same self-resetting behavior harness.lua
# already documented for this function), so this calls its `binToModel`
# directly instead — also file-scope-local, so reachable from here in the
# same chunk, and the one piece of dataToJson's preamble that actually
# populates keyP — then reads keyP immediately after, before anything has
# a chance to clear it.
sub get_extended_status {
    my ( $ip, $device_src ) = @_;
    my $device_info = DEVICE_INFO_LUA;
    my %status;

    for my $prop (@EXTENDED_QUERY_PROPS) {
        next if $prop eq EXTENDED_QUERY_ANCHOR;
        my $query_type = EXTENDED_QUERY_ANCHOR . ",$prop";

        my $hex = lua_run(
            $device_src,
            qq{local JSON = require "cjson"\n}
          . qq{local env = { deviceinfo = $device_info, query = { query_type = "$query_type" } }\n}
          . qq{return jsonToData(JSON.encode(env))}
        );

        my @bytes = unpack( 'C*', pack( 'H*', $hex ) );
        my $resp = request_with_retry(
            $ip, wrap_packet( \@bytes ),
            sub { defined $_[0]->[0x0a] && ( $_[0]->[0x0a] == 0xb0 || $_[0]->[0x0a] == 0xb1 ) }
        );

        my $json_text = lua_run(
            $device_src,
            qq{local JSON = require "cjson"\n}
          . qq{local info = string2table("} . ahex($resp) . qq{")\n}
          . qq{dataType = info[10]\n}
          . qq{local msgBytes = {}\n}
          . qq{for i = 1, #info do msgBytes[i - 1] = info[i] end\n}
          . qq{local msgLength = msgBytes[1]\n}
          . qq{local bodyLength = msgLength - keyB["BYTE_PROTOCOL_LENGTH"] - 1\n}
          . qq{local bodyBytes = {}\n}
          . qq{for i = 0, bodyLength do bodyBytes[i] = msgBytes[i + keyB["BYTE_PROTOCOL_LENGTH"]] end\n}
          . qq{binToModel(bodyBytes, nil)\n}
          . qq{return JSON.encode({ val = keyP["$prop"] })}
        );
        # A property the device didn't echo back leaves keyP[prop] nil,
        # so Lua's `{val = nil}` has no "val" key at all — cjson's own
        # ambiguous-empty-table heuristic then encodes it as "[]", not
        # "{}", so json_decode hands back an arrayref here, not a
        # hashref. Check the type rather than assume the shape.
        my $decoded = json_decode($json_text);
        $status{$prop} = $decoded->{val}
          if ref($decoded) eq 'HASH' && defined $decoded->{val};
    }

    return \%status;
}

sub cmd_get {
    my ( $ip, $device_src ) = @_;
    my $hex = ahex( get_status($ip) );
    my $device_info = DEVICE_INFO_LUA;

    my $json_text = lua_run(
        $device_src,
        qq{local JSON = require "cjson"\n}
      . qq{local env = { deviceinfo = $device_info, msg = { data = "$hex" } }\n}
      . qq{return dataToJson(JSON.encode(env))}
    );

    my $decoded = json_decode($json_text);
    my $status  = $decoded->{status} // {};

    # Extended properties are a best-effort add-on — a device that
    # rejects or doesn't answer the B1 query shouldn't take down the
    # regular status this command already has in hand.
    my $extended = eval { get_extended_status( $ip, $device_src ) };
    warn "extended properties unavailable: $@" if $@;
    %$status = ( %$status, %$extended ) if $extended;

    for my $prop ( sort keys %$status ) {
        printf "%s:%s\n", $prop, $status->{$prop};
    }
}

sub cmd_set {
    my ( $ip, $device_src, %want ) = @_;

    if ( exists $want{temperature} ) {
        my $val   = delete $want{temperature};
        my $whole = POSIX::floor($val);
        $want{temperature}       = $whole;
        $want{small_temperature} = ( $val - $whole >= 0.5 ) ? 0.5 : 0;
    }
    if ( exists $want{wind_speed} ) {
        my $val = $want{wind_speed};
        $want{wind_speed} = $WIND_SPEED_CODE{$val}
          // ( $val =~ /^\d+$/ ? $val : die "unknown wind_speed: $val\n" );
    }

    my %control      = ( %BASE_CONTROL, %want );
    my $control_lua  = lua_table_literal(%control);
    my $device_info  = DEVICE_INFO_LUA;

    my $hex = lua_run(
        $device_src,
        qq{local JSON = require "cjson"\n}
      . qq{local env = { deviceinfo = $device_info, control = $control_lua }\n}
      . qq{return jsonToData(JSON.encode(env))}
    );

    my @bytes = unpack( 'C*', pack( 'H*', $hex ) );
    my $resp = request_with_retry( $ip, wrap_packet( \@bytes ) );
    dbg( 'SET/ack', $resp );
    return 1;
}

# --- CLI ---
#
# Guarded by `unless (caller)` so t/ac_ng.t can `require` this file to
# exercise lua_run/lua_table_literal/cmd_get/cmd_set directly (against the
# real device .lua, no network) without Getopt::Long dying on missing
# --ip/--lua or the script trying to hit the network at require time.

unless (caller) {
    my %opt;
    Getopt::Long::GetOptions(
        \%opt,
        'ip=s', 'get', 'set', 'debug', 'help',
        'power=s', 'mode=s', 'temperature=f', 'wind_speed=s',
        map { "$_=s" } qw[
          wind_swing_lr wind_swing_ud strong_wind comfort_sleep purifier eco dry
          prevent_cold power_saving ptc buzzer tubro comfort_power_save
          natural_wind screen_display no_wind_sense fa_no_wind_sense security
          self_clean wind_avoid intelligent_wind child_prevent_cold_wind
          little_angel cool_hot_sense gentle_wind_sense even_wind single_tuyere
          extreme_wind voice_control pre_cool_hot nobody_energy_save water_pump
          intelligent_control prevent_super_cool wind_straight
          auto_prevent_straight_wind yb_wind_avoid
        ],
    ) or die "bad options\n";

    if ( $opt{help} || !$opt{ip} ) {
        print STDERR <<'USAGE';
Usage:
  ac-ng.pl --ip <addr> --get
  ac-ng.pl --ip <addr> --set --power on --mode cool --temperature 24

--get with no other flags decodes every field the device reports.

--set flags (names match the device script's own field names, not ac.pl's):
  --power            on|off
  --mode             auto|cool|dry|heat|fan
  --temperature      16..30 (step 0.5)
  --wind_speed       auto|high|medium|low|silent, or a raw number
  --<flag> on|off, one of:
    wind_swing_lr wind_swing_ud strong_wind comfort_sleep purifier eco dry
    prevent_cold power_saving ptc buzzer tubro comfort_power_save
    natural_wind screen_display no_wind_sense fa_no_wind_sense security
    self_clean wind_avoid intelligent_wind child_prevent_cold_wind
    little_angel cool_hot_sense gentle_wind_sense even_wind single_tuyere
    extreme_wind voice_control pre_cool_hot nobody_energy_save water_pump
    intelligent_control prevent_super_cool wind_straight
    auto_prevent_straight_wind yb_wind_avoid
USAGE
        exit( $opt{help} ? 0 : 2 );
    }

    $DEBUG = 1 if $opt{debug};

        my $device_src = do {
        binmode(DATA);
        local $/;
        <DATA>;
    };


    if ( $opt{get} ) {
        cmd_get( $opt{ip}, $device_src );
    }
    elsif ( $opt{set} ) {
        my %want;
        for my $k ( keys %opt ) {
            next if $k =~ /^(ip|get|set|debug|help)$/;
            $want{$k} = $opt{$k} if defined $opt{$k};
        }
        die "nothing to set (no property flags given)\n" unless %want;
        cmd_set( $opt{ip}, $device_src, %want );
        print "OK\n";
    }
    else {
        die "need --get or --set\n";
    }
}

1;
__DATA__
----海外空调协议解析,从国内61版本拷贝
----author: Li Tong
----date  : 2023/01/08

--CA机型，04,05上报的温度 要按小数位在BIT1解释
--正常机型, 04,05上报的温度 要按小数位在BIT6解释

local JSON = require "cjson"

-----------------JSON相关key值变量-----------------
local keyT = {}

--版本号
keyT["KEY_VERSION"] = "version"
--电源
keyT["KEY_POWER"] = "power"
--净化
keyT["KEY_PURIFIER"] = "purifier"
--模式
keyT["KEY_MODE"] = "mode"
--智能抽湿值
keyT["KEY_SMART_DRY"] = "smart_dry_value"
--温度
keyT["KEY_TEMPERATURE"] = "temperature"
--风速
keyT["KEY_FANSPEED"] = "wind_speed"
--左右扫风
keyT["KEY_SWING_LR"] = "wind_swing_lr"
--上下扫风
keyT["KEY_SWING_UD"] = "wind_swing_ud"
--下左右扫风
keyT["KEY_SWING_LR_UNDER"] = "wind_swing_lr_under"
--定时开
keyT["KEY_TIME_ON"] = "power_on_timer"
--定时关
keyT["KEY_TIME_OFF"] = "power_off_timer"
--定时关时间
keyT["KEY_CLOSE_TIME"] = "power_off_time_value"
--定时开时间
keyT["KEY_OPEN_TIME"] = "power_on_time_value"
--ECO
keyT["KEY_ECO"] = "eco"
--干燥
keyT["KEY_DRY"] = "dry"
--电辅热
keyT["KEY_PTC"] = "ptc"
--本次开机运行时间
keyT["KEY_CURRENT_WORK_TIME"] = "current_work_time"
--错误码
keyT["KEY_ERROR_CODE"] = "error_code"
--按键（蜂鸣）
keyT["KEY_BUZZER"] = "buzzer"
--防过冷(快速降温，缓慢回温)
keyT["KEY_PREVENT_SUPER_COOL"] = "prevent_super_cool"
--防着凉
keyT["KEY_PREVENT_COLD"] = "prevent_cold"
--防直吹
keyT["KEY_PREVENT_STRAIGHT_WIND"] = "prevent_straight_wind"
--自动防直吹
keyT["KEY_AUTO_PREVENT_STRAIGHT_WIND"] = "auto_prevent_straight_wind"
--自清洁
keyT["KEY_SELF_CLEAN"] = "self_clean"
--风吹人
keyT["KEY_WIND_STRAIGHT"] = "wind_straight"
--风避人
keyT["KEY_WIND_AVOID"] = "wind_avoid"
--智慧风
keyT["KEY_INTELLIGENT_WIND"] = "intelligent_wind"
--无风感
keyT["KEY_NO_WIND_SENSE"] = "no_wind_sense"
--远近无风感
keyT["KEY_FA_NO_WIND_SENSE"] = "fa_no_wind_sense"
--儿童放冷风
keyT["KEY_CHILD_PREVENT_COLD_WIND"] = "child_prevent_cold_wind"
--强劲
keyT["KEY_STRONG_WIND"] = "strong_wind"
--Tubro
keyT["KEY_TUBRO"] = "tubro"
--舒省
keyT["KEY_COMFORT_POWER_SAVE"] = "comfort_power_save"
--屏显
keyT["KEY_SCREEN_DISPLAY"] = "screen_display"
--当前屏显状态
keyT["KEY_SCREEN_DISPLAY_NOW"] = "screen_display_now"
--小天使
keyT["KEY_LITTLE_ANGLE"] = "little_angel"
--冷热感
keyT["KEY_COOL_HOT_SENSE"] = "cool_hot_sense"
--柔风感
keyT["KEY_GENTLE_WIND_SENSE"] = "gentle_wind_sense"
--安防
keyT["KEY_SECURITY"] = "security"
--均匀风
keyT["KEY_EVEN_WIND"] = "even_wind"
--单风口
keyT["KEY_SINGLE_TUYERE"] = "single_tuyere"
--超远风
keyT["KEY_EXTREME_WIND"] = "extreme_wind"
--语音控制
keyT["KEY_VOICE_CONTROL"] = "voice_control"
--舒睡
keyT["KEY_COMFORT_SLEEP"] = "comfort_sleep"
--舒睡曲线
keyT["KEY_COMFORT_SLEEP_CURVE"] = "comfort_sleep_curve"
--预冷预热
keyT["KEY_PRE_COOL_HOT"] = "pre_cool_hot"
--自然风
keyT["KEY_NATURAL_WIND"] = "natural_wind"
--pmv
keyT["KEY_PMV"] = "pmv"
--水洗
keyT["KEY_WATER_WASHING"] = "water_washing"
--新风
keyT["KEY_FRESH_AIR"] = "fresh_air"
--yb系列风避人
keyT["KEY_YB_WIND_AVOID"] = "yb_wind_avoid"
--fa系列防直吹
keyT["KEY_FA_PREVENT_STRAIGHT_WIND"] = "fa_prevent_straight_wind"
--家长控制
keyT["KEY_PARENT_CONTROL"] = "parent_control"
--无人节能
keyT["KEY_NOBODY_ENERGY_SAVE"] = "nobody_energy_save"
--上下摆风角度
keyT["KEY_WIND_SWING_UD_ANGLE"] = "wind_swing_ud_angle"
--左右摆风角度
keyT["KEY_WIND_SWING_LR_ANGLE"] = "wind_swing_lr_angle"
--滤网脏堵检测值
keyT["KEY_FILTER_VALUE"] = "filter_value"
--滤网脏堵等级
keyT["KEY_FILTER_LEVEL"] = "filter_level"
--左右防直吹
keyT["KEY_PREVENT_STRAIGHT_WIND_LR"] = "prevent_straight_wind_lr"
--pm25值
keyT["KEY_PM25_VALUE"] = "pm25_value"
--水泵开关
keyT["KEY_WATER_PUMP"] = "water_pump"
--智能控制总开关
keyT["KEY_INTENLLIGENT_CONTROL"] = "intelligent_control"
--音量控制
keyT["KEY_VOLUME_CONTROL"] = "volume_control"
--语音控制开关（新）
keyT["KEY_VOICE_CONTROL_NEW"] = "voice_control_new"
--人脸注册人数
keyT["KEY_FACE_REGISTER"] = "face_register"
--温度上下限
keyT["KEY_COOL_TEMP_UP"] = "cool_temp_up"
keyT["KEY_COOL_TEMP_DOWN"] = "cool_temp_down"
keyT["KEY_AUTO_TEMP_UP"] = "auto_temp_up"
keyT["KEY_AUTO_TEMP_DOWN"] = "auto_temp_down"
keyT["KEY_HEAT_TEMP_UP"] = "heat_temp_up"
keyT["KEY_HEAT_TEMP_DOWN"] = "heat_temp_down"
--省电
keyT["KEY_POWER_SAVING"] = "power_saving"
--遥控器锁定
keyT["KEY_REMOTE_CONTROL_LOCK"] = "remote_control_lock"
--空调允许运行时间
keyT["KEY_OPERATING_TIME"] = "operating_time"
--新风滤网总时长
keyT["KEY_FRESH_FILTER_TIME_TOTAL"] = "fresh_filter_time_total"
--新风滤网已使用时长
keyT["KEY_FRESH_FILTER_TIME_USE"] = "fresh_filter_time_use"
--新风滤网使用超时标志
keyT["KEY_FRESH_FILTER_TIMEOUT"] = "fresh_filter_timeout"
--新风滤网使用超时标志(AE2)
keyT["KEY_FRESH_FILTER_TIMEOUT_AE2"] = "fresh_filter_timeout_ae2"
--新风滤网运行时间清零
keyT["KEY_FRESH_FILTER_RESET"] = "fresh_filter_reset"
--普通滤网运行时间清零
keyT["KEY_COMMON_FILTER_RESET"] = "common_filter_reset"
--室内湿度
keyT["KEY_INDOOR_HUMIDITY"] = "indoor_humidity"
--8度制热
keyT["KEY_DEGREE8_HEAT"] = "degree8_heat"

----------------JSON相关value值变量----------------
local keyV = {}
--版本号
keyV["VALUE_VERSION"] = 51
--功能开
keyV["VALUE_FUNCTION_ON"] = "on"
--功能关
keyV["VALUE_FUNCTION_OFF"] = "off"
--制热
keyV["VALUE_MODE_HEAT"] = "heat"
--制冷
keyV["VALUE_MODE_COOL"] = "cool"
--自动
keyV["VALUE_MODE_AUTO"] = "auto"
--干燥
keyV["VALUE_MODE_DRY"] = "dry"
--送风
keyV["VALUE_MODE_FAN"] = "fan"
--智能除湿
keyV["VALUE_MODE_SMART_DRY"] = "smart_dry"
--室内温度
keyV["VALUE_INDOOR_TEMPERATURE"] = "indoor_temperature"
--室外温度
keyV["VALUE_OUTDOOR_TEMPERATURE"] = "outdoor_temperature"
--运行状态
keyV["VALUE_RUN_STATE"] = "runstate"
--运行
keyV["VALUE_RUNNING"] = "running"
--停止
keyV["VALUE_STOP"] = "stopped"

local deviceSubType=0
local deviceSN8="00000000"
-----------------二进制相关属性变量----------------
local keyB = {}
--设备
keyB["BYTE_DEVICE_TYPE"] = 0xAC
--控制请求
keyB["BYTE_CONTROL_REQUEST"] = 0x02
--查询请求
keyB["BYTE_QUERYL_REQUEST"] = 0x03
--协议头
keyB["BYTE_PROTOCOL_HEAD"] = 0xAA
--协议头长度
keyB["BYTE_PROTOCOL_LENGTH"] = 0x0A
--电源开
keyB["BYTE_POWER_ON"] = 0x01
--电源关
keyB["BYTE_POWER_OFF"] = 0x00

--自动模式
keyB["BYTE_MODE_AUTO"] = 0x20
--制冷模式
keyB["BYTE_MODE_COOL"] = 0x40
--抽湿模式
keyB["BYTE_MODE_DRY"] = 0x60
--制热模式
keyB["BYTE_MODE_HEAT"] = 0x80
--送风模式
keyB["BYTE_MODE_FAN"] = 0xA0
--智能抽湿模式
keyB["BYTE_MODE_SMART_DRY"] = 0xC0

--自动风
keyB["BYTE_FANSPEED_AUTO"] = 0x66
--高风
keyB["BYTE_FANSPEED_HIGH"] = 0x50
--中风
keyB["BYTE_FANSPEED_MID"] = 0x3C
--低风
keyB["BYTE_FANSPEED_LOW"] = 0x28
--微风
keyB["BYTE_FANSPEED_MUTE"] = 0x14
--净化开
keyB["BYTE_PURIFIER_ON"] = 0x20
--净化关
keyB["BYTE_PURIFIER_OFF"] = 0x00
--经济（ECO）开
keyB["BYTE_ECO_ON"] = 0x80
--经济（ECO）关
keyB["BYTE_ECO_OFF"] = 0x00
--左右扫风开
keyB["BYTE_SWING_LR_ON"] = 0x03
--左右扫风关
keyB["BYTE_SWING_LR_OFF"] = 0x00
--下左右扫风开
keyB["BYTE_SWING_LR_UNDER_ON"] = 0x80
--下左右扫风关
keyB["BYTE_SWING_LR_UNDER_OFF"] = 0x00
--下左右扫风功能开
keyB["BYTE_SWING_LR_UNDER_ENABLE"] = 0x80
--下左右扫风功能关
keyB["BYTE_SWING_LR_UNDER_DISABLE"] = 0x00
--上下扫风开
keyB["BYTE_SWING_UD_ON"] = 0x0C
--上下扫风关
keyB["BYTE_SWING_UD_OFF"] = 0x00
--干燥开
keyB["BYTE_DRY_ON"] = 0x04
--干燥关
keyB["BYTE_DRY_OFF"] = 0x00
--buzzer（蜂鸣）开
keyB["BYTE_BUZZER_ON"] = 0x40
--buzzer（蜂鸣）关
keyB["BYTE_BUZZER_OFF"] = 0x00
--设备控制命令
keyB["BYTE_CONTROL_CMD"] = 0x40
--定时方式(相对)
keyB["BYTE_TIMER_METHOD_REL"] = 0x00
--定时方式(相对)
keyB["BYTE_TIMER_METHOD_ABS"] = 0x01
--定时方式(禁用)
keyB["BYTE_TIMER_METHOD_DISABLE"] = 0x7F
--移动终端控制
keyB["BYTE_CLIENT_MODE_MOBILE"] = 0x02
--移动端定时开
keyB["BYTE_TIMER_SWITCH_ON"] = 0x80
--移动端定时关
keyB["BYTE_TIMER_SWITCH_OFF"] = 0x00
--定时关(开)
keyB["BYTE_CLOSE_TIMER_SWITCH_ON"] = 0x80
--定时关(关)
keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"] = 0x7F
--定时开(开)
keyB["BYTE_START_TIMER_SWITCH_ON"] = 0x80
--定时开(关)
keyB["BYTE_START_TIMER_SWITCH_OFF"] = 0x7F
--PTC（电辅热）开
keyB["BYTE_PTC_ON"] = 0x08
--PTC（电辅热）关
keyB["BYTE_PTC_OFF"] = 0x00
--强劲风开
keyB["BYTE_STRONG_WIND_ON"] = 0x20
--强劲风关
keyB["BYTE_STRONG_WIND_OFF"] = 0x00
--舒睡开
keyB["BYTE_SLEEP_ON"] = 0x03
--舒睡关
keyB["BYTE_SLEEP_OFF"] = 0x00
--舒省开
keyB["BYTE_COMFORT_POWER_SAVE_ON"] = 0x01
--舒省关
keyB["BYTE_COMFORT_POWER_SAVE_OFF"] = 0x00
--均匀风开
keyB["BYTE_EVEN_WIND_ON"] = 0x01
--均匀风关
keyB["BYTE_EVEN_WIND_OFF"] = 0x00 
--单风口开
keyB["BYTE_SINGLE_TUYERE_ON"] = 0x01
--单风口关
keyB["BYTE_SINGLE_TUYERE_OFF"] = 0x00
--超远风开
keyB["BYTE_EXTREME_WIND_ON"] = 0x01
--超远风关
keyB["BYTE_EXTREME_WIND_OFF"] = 0x00
--语音控制开
keyB["BYTE_VOICE_CONTROL_ON"] = 0x03
--语音控制关
keyB["BYTE_VOICE_CONTROL_OFF"] = 0x00
--自然风开
keyB["BYTE_NATURAL_WIND_ON"] = 0x40
--自然风关
keyB["BYTE_NATURAL_WIND_OFF"] = 0x00

--设备控制命令
keyB["BYTE_CONTROL_PROPERTY_CMD"] = 0xB0

-------------------定义属性变量--------------------
local keyP = {}
local dataType = 0
local comfortByte = nil

local function init_keyP()
	keyP["powerValue"] = nil 
	keyP["modeValue"] = nil 
	keyP["smartDryValue"] = nil 
	keyP["temperature"] = nil 
	keyP["smallTemperature"] = nil 
	keyP["indoorTemperatureValue"] = nil 
	keyP["smallIndoorTemperatureValue"] = nil
	keyP["outdoorTemperatureValue"] = nil 
	keyP["smallOutdoorTemperatureValue"] = nil
	keyP["fanspeedValue"] = nil 
	keyP["closeTimerSwitch"] = nil 
	keyP["openTimerSwitch"] = nil 
	keyP["closeHour"] = nil
	keyP["closeStepMintues"] = nil
	keyP["closeMin"] = nil
	keyP["closeTime"] = nil
	keyP["openHour"] = nil
	keyP["openStepMintues"] = nil
	keyP["openMin"] = nil
	keyP["openTime"] = nil
	keyP["strongWindValue"] = nil
	keyP["tubroValue"] = nil
	keyP["comfortableSleepValue"] = nil
	keyP["comfortableSleepSwitch"] = nil
	keyP["comfortableSleepTime"] = nil
	keyP["comfort_sleep_curve"] = nil
	keyP["PTCValue"] = nil
	keyP["purifierValue"] = nil 
	keyP["ecoValue"] = nil 
	keyP["dryValue"] = nil 
	keyP["swingLRValue"] = nil 
	keyP["swingUDValue"] = nil 
	keyP["swingLRValueUnder"] = 0
    keyP["swingLRUnderSwitch"] = 0
	keyP["currentWorkTime"] = nil
	keyP["PTCForceValue"] = 0
	keyP["screenDisplayNowValue"] = nil
	keyP["temperature_unit"] = nil
	--美居之前就是默认开的
	keyP["buzzerValue"] = 0x40 
	keyP["errorCode"] = nil
	--是否踢被子
	keyP["kickQuilt"]= nil
	--防着凉
	keyP["preventCold"] = nil
	--舒省
	keyP["comfortPowerSave"] = nil
	--自然风
	keyP["naturalWind"] = nil
	--pmv
	keyP["pmv"] = nil
	--新风滤网
	keyP["fresh_filter_time_total"] = nil
	keyP["fresh_filter_time_use"] = nil
	keyP["fresh_filter_timeout"] = nil
	keyP["fresh_filter_timeout_ae2"] = nil
	keyP["fresh_filter_reset"] = nil
	keyP["common_filter_reset"] = nil
	keyP["degree8_heat"] = nil

	--新协议，变长属性解析协议
	keyP["propertyNumber"] = 0
	keyP["prevent_super_cool"] = nil 
	keyP["prevent_straight_wind"] = nil 
	keyP["auto_prevent_straight_wind"] = nil 
	keyP["self_clean"] = nil 
	keyP["wind_straight"] = nil 
	keyP["wind_avoid"] = nil
    keyP["yb_wind_avoid"] = nil	
	keyP["intelligent_wind"] = nil 
	keyP["no_wind_sense"] = nil 
	keyP["child_prevent_cold_wind"] = nil 
	keyP["little_angel"] = nil 
	keyP["cool_hot_sense"] = nil 
	keyP["gentle_wind_sense"] = nil 
	keyP["security"] = nil
	keyP["even_wind"] = nil
	keyP["single_tuyere"] = nil
	keyP["extreme_wind"] = nil
	keyP["extreme_wind_level"] = nil
	keyP["voice_control"] = nil
	keyP["pre_cool_hot"] = nil
	keyP["water_washing"] = nil
	keyP["fresh_air"] = nil
	keyP["no_wind_sense_level"] = nil
	keyP["fa_no_wind_sense"] = nil
	keyP["fa_prevent_straight_wind"] = nil
	keyP["parent_control"] = nil
	keyP["parent_control_temp_up"] = nil
	keyP["parent_control_temp_down"] = nil
	keyP["nobody_energy_save"] = nil
	keyP["filter_value"] = nil
	keyP["filter_level"] = nil
	keyP["prevent_straight_wind_lr"] = nil
	keyP["pm25_value"] = nil
	keyP["water_pump"] = nil
	keyP["intelligent_control"] = nil
	keyP["wind_swing_ud_angle"] = nil
	keyP["wind_swing_lr_angle"] = nil
	keyP["volume_control"] = nil
	keyP["voice_control_new"] = nil
	keyP["face_register"] = nil
	keyP["cool_temp_up"] = nil
	keyP["cool_temp_down"] = nil
	keyP["auto_temp_up"] = nil
	keyP["auto_temp_down"] = nil
	keyP["heat_temp_up"] = nil
	keyP["heat_temp_down"] = nil
	keyP["power_saving"] = nil
	keyP["remote_control_lock"] = nil
	keyP["remote_control_lock_control"] = nil
	keyP["indoor_humidity"] = nil
	keyP["b5_mode"] = nil
	keyP["b5_strong_wind"] = nil
	keyP["b5_wind_speed"] = nil
	keyP["b5_humidity"] = nil
	keyP["b5_temperature_0"] = nil
	keyP["b5_temperature_1"] = nil
	keyP["b5_temperature_2"] = nil
	keyP["b5_temperature_3"] = nil
	keyP["b5_temperature_4"] = nil
	keyP["b5_temperature_5"] = nil
	keyP["b5_temperature_6"] = nil
	keyP["b5_eco"] = nil
	keyP["b5_filter_remind"] = nil
	keyP["b5_filter_check"] = nil
	keyP["b5_fahrenheit"] = nil
	keyP["b5_8_heat"] = nil
	keyP["b5_electricity"] = nil
	keyP["b5_ptc"] = nil
	keyP["b5_wind_straight"] = nil
	keyP["b5_wind_avoid"] = nil
	keyP["b5_wind_swing"] = nil
	keyP["b5_no_wind_sense"] = nil
	keyP["b5_screen_display"] = nil
	keyP["b5_anion"] = nil
	keyP["b5_self_clean"] = nil
	keyP["b5_fa_no_wind_sense"] = nil
	keyP["b5_nobody_energy_save"] = nil
	keyP["b5_prevent_straight_wind"] = nil
	keyP["real_time_power"] = nil
	keyP["real_time_power_10"] = nil
	keyP["current_humidity"] = nil
	keyP["prevent_straight_wind_flag"] = nil
	keyP["fa_gentle_wind_sense"] = nil
	keyP["jet_cool"] = nil
	keyP["b5_jet_cool"] = nil
	keyP["body_check"] = nil
	keyP["b5_body_check"] = nil
	keyP["rate_select"] = nil
	keyP["b5_rate_select"] = nil
	keyP["b5_fresh_air"] = nil
	keyP["b5_wind_swing_lr_angle"] = nil
	keyP["b5_wind_swing_ud_angle"] = nil
	keyP["main_horizontal_guide_strip_1"] = nil
	keyP["main_horizontal_guide_strip_2"] = nil
	keyP["main_horizontal_guide_strip_3"] = nil
	keyP["main_horizontal_guide_strip_4"] = nil
	keyP["sup_horizontal_guide_strip_1"] = nil
	keyP["sup_horizontal_guide_strip_2"] = nil
	keyP["sup_horizontal_guide_strip_3"] = nil
	keyP["sup_horizontal_guide_strip_4"] = nil
	keyP["twins_machine"] = nil
	keyP["guide_strip_type"] = nil
	keyP["b5_main_horizontal_guide_strip_1"] = nil
	keyP["b5_main_horizontal_guide_strip_2"] = nil
	keyP["b5_main_horizontal_guide_strip_3"] = nil
	keyP["b5_main_horizontal_guide_strip_4"] = nil
	keyP["b5_sup_horizontal_guide_strip_1"] = nil
	keyP["b5_sup_horizontal_guide_strip_2"] = nil
	keyP["b5_sup_horizontal_guide_strip_3"] = nil
	keyP["b5_sup_horizontal_guide_strip_4"] = nil
	keyP["b5_twins_machine"] = nil
	keyP["b5_guide_strip_type"] = nil
	keyP["main_strip_control"] = nil
	keyP["sup_strip_control"] = nil
	keyP["sleep_status"] = nil
	keyP["sound"] = nil
	keyP["b5_sound"] = nil
	keyP["anion"] = nil
	keyP["b5_anion"] = nil
	keyP["machine_type"] = nil
	keyP["product_type"] = nil
	keyP["independent_ptc"] = nil
	keyP["gen_mode"] = nil
	keyP["b5_parent_control"] = nil
	
	--ieco
	keyP["ieco_switch"]= nil
	keyP["ieco_frame"]= nil
	keyP["ieco_target_rate"]= nil
	keyP["ieco_indoor_wind_speed"]= nil
	keyP["ieco_outdoor_wind_speed"]= nil
	keyP["ieco_expansion_valve"]= nil
	keyP["b5_ieco_switch"]= nil
	keyP["ieco_indoor_wind_speed_level"]= nil
	keyP["ieco_outdoor_wind_speed_level"]= nil
	keyP["ieco_number"]= nil
	--2023-01-09
	keyP["wind_around"] = nil
	keyP["wind_around_ud"] = nil
	keyP["b5_wind_around"] = nil
	keyP["prevent_straight_wind_select"] = nil
	keyP["b5_prevent_straight_wind_select"] = nil
	
	--2023-03-24
	keyP["mito_cool"] = nil
	keyP["mito_heat"] = nil
	keyP["dr_time_hour"] = nil
	keyP["dr_time_min"] = nil
	keyP["t2_heat"] = nil
	keyP["tp_heat"] = nil
	keyP["k1_value"] = nil
	keyP["k2_value"] = nil
	keyP["k3_value"] = nil
	keyP["k4_value"] = nil
	keyP["cool_strong_wind_speed"] = nil
	keyP["cool_strong_wind_amount"] = nil
	keyP["has_cool_heat_amount"] = nil
	
	--2023-06-15
	keyP["has_icheck"] = nil
	keyP["b5_has_icheck"] = nil
	keyP["b5_emergent_heat_wind"] = nil
	keyP["b5_heat_ptc_wind"] = nil
	keyP["cvp"] = nil
	keyP["b5_cvp"] = nil
	
	keyP["b5_new_wind_sense"] = nil
	keyP["new_wind_sense"] = nil
	
	keyP["in_code"] = nil
	keyP["in_version"] = nil
	keyP["out_code"] = nil
	keyP["out_version"] = nil
	keyP["comfort"] = nil
	keyP["b5_air_ieco"] = nil
	keyP["b5_end_ieco"] = nil
end

init_keyP()
local propertyPre = nil

---------------公共的函数 begin---------------
--打印 table 表
local function  print_lua_table(lua_table, indent)
    indent = indent or 0
    
    for k, v in pairs(lua_table) do
        if type(k) == "string" then
            k = string.format("%q", k)
        end
        
        local szSuffix = ""
        
        if type(v) == "table" then
            szSuffix = "{"
        end
        
        local szPrefix = string.rep("    ", indent)
        formatting = szPrefix.."["..k.."]".." = "..szSuffix
        
        if type(v) == "table" then
            print(formatting)
            
            print_lua_table(v, indent + 1)
            
            print(szPrefix.."},")
        else
            local szValue = ""
            
            if type(v) == "string" then
                szValue = string.format("%q", v)
            else
                szValue = tostring(v)
            end
            
            print(formatting..szValue..",")
        end
    end
end

--检查取值是否超过边界
local function  checkBoundary(data, min, max)
    if (not data) then
        data = 0
    end
    
    data = tonumber(data)

    if(data == nil) then
        data = 0
    end
    
    if ((data >= min) and (data <= max)) then
        return data
    else
        if (data < min) then
            return min
        else
            return max
        end
    end
end

--table 转 string
local function  table2string(cmd)
    local ret = ""
    local i
    
    for i = 1, #cmd do
        ret = ret..string.char(cmd[i])
    end
    
    return ret
end

--十六进制 string 转 table
local function  string2table(hexstr)
    local tb = {}
    local i = 1
    local j = 1
    
    for i = 1, #hexstr - 1, 2 do
        local doublebytestr = string.sub(hexstr, i, i + 1)
        tb[j] = tonumber(doublebytestr, 16)
        j = j + 1
    end
    
    return tb
end

--string 转 table
local function  numstring2table(hexstr)
    local tb = {}
    local i = 1
    local j = 1
    
    for i = 1, #hexstr - 1, 2 do
        local doublebytestr = string.sub(hexstr, i, i + 1)
        tb[j] = doublebytestr
        j = j + 1
    end
    
    return tb
end

--十六进制 string 输出
local function  string2hexstring(str)
    local ret = ""
    
    for i = 1, #str do
        ret = ret .. string.format("%02x", str:byte(i))
    end
    
    return ret
end

--table 转 json
local function  encode(cmd)
    local tb
    
    if JSON == nil then
        JSON = require "cjson"
    end
    
    tb = JSON.encode(cmd)
    
    return tb
end

--json 转 table
local function  decode(cmd)
    local tb
    
    if JSON == nil then
        JSON = require "cjson"
    end
    
    tb = JSON.decode(cmd)
    
    return tb
end

--BCD转码
local function bcd2Int(bcd)
	return (bit.band(0x0F,bit.rshift(bcd,4))) * 10 + bit.band(0x0F,bcd)
end

--sum校验
local function  makeSum(tmpbuf, start_pos, end_pos)
    local resVal = 0
    
    for si = start_pos, end_pos do
        resVal = resVal + tmpbuf[si]
        
        if resVal > 0xff then
            resVal = bit.band(resVal, 0xff)
        end
    end
    
    resVal = bit.band(255 - resVal + 1, 0xff)
    
    return resVal
end

local function splitStrByChar(str,sepChar)
	local splitList = {}
	local pattern = '[^'..sepChar..']+'
		string.gsub(str, pattern, function(w) table.insert(splitList, w) end )
	return splitList
end

local function values (t)
	local i = 0
	return function() i = i + 1; return t[i] end
end

local function convert_to_F(temp)
	local tempTable = {}
	tempTable[10] = 50
	tempTable[10.5] = 51
	tempTable[11] = 52
	tempTable[11.5] = 53
	tempTable[12] = 54
	tempTable[12.5] = 54
	tempTable[13] = 55
	tempTable[13.5] = 56
	tempTable[14] = 57
	tempTable[14.5] = 58
	tempTable[15] = 59
	tempTable[15.5] = 59
	tempTable[16] = 60
	tempTable[16.5] = 61
	tempTable[17] = 62
	tempTable[17.5] = 63
	tempTable[18] = 64
	tempTable[18.5] = 65
	tempTable[19] = 66
	tempTable[19.5] = 67
	tempTable[20] = 68
	tempTable[20.5] = 69
	tempTable[21] = 70
	tempTable[21.5] = 71
	tempTable[22] = 72
	tempTable[22.5] = 73
	tempTable[23] = 73
	tempTable[23.5] = 74
	tempTable[24] = 75
	tempTable[24.5] = 76
	tempTable[25] = 77
	tempTable[25.5] = 78
	tempTable[26] = 79
	tempTable[26.5] = 80 
	tempTable[27] = 81
	tempTable[27.5] = 82
	tempTable[28] = 82
	tempTable[28.5] = 83
	tempTable[29] = 84
	tempTable[29.5] = 85
	tempTable[30] = 86
	tempTable[30.5] = 87
	tempTable[31] = 88
	tempTable[31.5] = 89

	local temperature = temp
	if(type(temp) == "string")then
		temperature = tonumber(temp)
	end
	return tempTable[temperature]
end

local function convert_to_C(temp)
	local tempTable = {}
	tempTable[50] = 10
	tempTable[51] = 10.5
	tempTable[52] = 11
	tempTable[53] = 11.5
	tempTable[54] = 12
	tempTable[55] = 13
	tempTable[56] = 13.5
	tempTable[57] = 14
	tempTable[58] = 14.5
	tempTable[59] = 15
	tempTable[60] = 16
	tempTable[61] = 16.5
	tempTable[62] = 17
	tempTable[63] = 17.5
	tempTable[64] = 18
	tempTable[65] = 18.5
	tempTable[66] = 19
	tempTable[67] = 19.5
	tempTable[68] = 20
	tempTable[69] = 20.5
	tempTable[70] = 21
	tempTable[71] = 21.5
	tempTable[72] = 22
	tempTable[73] = 23
	tempTable[74] = 23.5
	tempTable[75] = 24
	tempTable[76] = 24.5
	tempTable[77] = 25
	tempTable[78] = 25.5
	tempTable[79] = 26
	tempTable[80] = 26.5
	tempTable[81] = 27
	tempTable[82] = 28
	tempTable[83] = 28.5
	tempTable[84] = 29
	tempTable[85] = 29.5
	tempTable[86] = 30 
	tempTable[87] = 30.5
	tempTable[88] = 31
	tempTable[89] = 31.5
	local temperature = temp
	if(type(temp) == "string")then
		temperature = tonumber(temp)
	end
	return tempTable[temperature]
end
--CRC表
local crc8_854_table =
{
    0, 94, 188, 226, 97, 63, 221, 131, 194, 156, 126, 32, 163, 253, 31, 65,
    157, 195, 33, 127, 252, 162, 64, 30, 95, 1, 227, 189, 62, 96, 130, 220,
    35, 125, 159, 193, 66, 28, 254, 160, 225, 191, 93, 3, 128, 222, 60, 98,
    190, 224, 2, 92, 223, 129, 99, 61, 124, 34, 192, 158, 29, 67, 161, 255,
    70, 24, 250, 164, 39, 121, 155, 197, 132, 218, 56, 102, 229, 187, 89, 7,
    219, 133, 103, 57, 186, 228, 6, 88, 25, 71, 165, 251, 120, 38, 196, 154,
    101, 59, 217, 135, 4, 90, 184, 230, 167, 249, 27, 69, 198, 152, 122, 36,
    248, 166, 68, 26, 153, 199, 37, 123, 58, 100, 134, 216, 91, 5, 231, 185,
    140, 210, 48, 110, 237, 179, 81, 15, 78, 16, 242, 172, 47, 113, 147, 205,
    17, 79, 173, 243, 112, 46, 204, 146, 211, 141, 111, 49, 178, 236, 14, 80,
    175, 241, 19, 77, 206, 144, 114, 44, 109, 51, 209, 143, 12, 82, 176, 238,
    50, 108, 142, 208, 83, 13, 239, 177, 240, 174, 76, 18, 145, 207, 45, 115,
    202, 148, 118, 40, 171, 245, 23, 73, 8, 86, 180, 234, 105, 55, 213, 139,
    87, 9, 235, 181, 54, 104, 138, 212, 149, 203, 41, 119, 244, 170, 72, 22,
    233, 183, 85, 11, 136, 214, 52, 106, 43, 117, 151, 201, 74, 20, 246, 168,
    116, 42, 200, 150, 21, 75, 169, 247, 182, 232, 10, 84, 215, 137, 107, 53
}

--CRC校验
local function  crc8_854(dataBuf, start_pos, end_pos)
    local crc = 0
    
    for si = start_pos, end_pos do
        crc = crc8_854_table[bit.band(bit.bxor(crc, dataBuf[si]), 0xFF) + 1]
    end
    
    return crc
end

---------------公共的函数 end---------------


-----------根据电控协议不同，需要改变的函数-------------
--根据 json 修改属性变量
local function  jsonToModel(jsonCmd,jsonType)
    local streams = jsonCmd
    
    --蜂鸣，定时方式，遥控器端来源，电源
    if (streams[keyT["KEY_POWER"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["powerValue"] = keyB["BYTE_POWER_ON"]
		keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_OFF"]
		keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]
		keyP["PTCValue"] = keyB["BYTE_PTC_ON"]
    elseif (streams[keyT["KEY_POWER"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["powerValue"] = keyB["BYTE_POWER_OFF"]
		keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_OFF"]
		keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]
		keyP["PTCValue"] = keyB["BYTE_PTC_ON"]
    end
	
	--按键（按键有无用于决定空调器蜂鸣器是否发出声音）
	if (streams[keyT["KEY_BUZZER"]] == "VALUE_FUNCTION_ON") then
        keyP["buzzerValue"] = keyB["BYTE_BUZZER_ON"]
    elseif (streams[keyT["KEY_BUZZER"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["buzzerValue"] = keyB["BYTE_BUZZER_OFF"]
    end

    --净化
    if (streams[keyT["KEY_PURIFIER"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["purifierValue"] = keyB["BYTE_PURIFIER_ON"]
    elseif (streams[keyT["KEY_PURIFIER"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["purifierValue"] = keyB["BYTE_PURIFIER_OFF"]
    end

    --ECO
    if (streams[keyT["KEY_ECO"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["ecoValue"] = keyB["BYTE_ECO_ON"]
    elseif (streams[keyT["KEY_ECO"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["ecoValue"] = keyB["BYTE_ECO_OFF"]
    end

    --干燥
    if (streams[keyT["KEY_DRY"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["dryValue"] = keyB["BYTE_DRY_ON"]
    elseif (streams[keyT["KEY_DRY"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["dryValue"] = keyB["BYTE_DRY_OFF"]
    end

    --模式和温度
    if (streams[keyT["KEY_MODE"]] == keyV["VALUE_MODE_HEAT"]) then
        keyP["modeValue"] = keyB["BYTE_MODE_HEAT"]
		keyP["PTCValue"] = keyB["BYTE_PTC_ON"]
    elseif (streams[keyT["KEY_MODE"]] == keyV["VALUE_MODE_COOL"]) then
        keyP["modeValue"] = keyB["BYTE_MODE_COOL"]
    elseif (streams[keyT["KEY_MODE"]] == keyV["VALUE_MODE_AUTO"]) then
        keyP["modeValue"] = keyB["BYTE_MODE_AUTO"]
		keyP["PTCValue"] = keyB["BYTE_PTC_ON"]
    elseif (streams[keyT["KEY_MODE"]] == keyV["VALUE_MODE_DRY"]) then
        keyP["modeValue"] = keyB["BYTE_MODE_DRY"]
    elseif (streams[keyT["KEY_MODE"]] == keyV["VALUE_MODE_FAN"]) then
        keyP["modeValue"] = keyB["BYTE_MODE_FAN"]
    elseif (streams[keyT["KEY_MODE"]] == keyV["VALUE_MODE_SMART_DRY"]) then
        keyP["modeValue"] = keyB["BYTE_MODE_SMART_DRY"]
	elseif (streams["mode"] == "single_elecheat")then
		keyP["modeValue"] = keyB["BYTE_MODE_HEAT"]
		keyP["independent_ptc"] = 0x08
	elseif (streams["mode"] == "heat_elecheat")then
		keyP["modeValue"] = keyB["BYTE_MODE_HEAT"]
		keyP["PTCValue"] = keyB["BYTE_PTC_ON"]
    end
    --智能除湿值
    if (streams[keyT["KEY_SMART_DRY"]] ~= nil) then
		keyP["smartDryValue"] = checkBoundary(streams[keyT["KEY_SMART_DRY"]], 30, 101)
	end
    --自然风
    if (streams[keyT["KEY_NATURAL_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["naturalWind"] = keyB["BYTE_NATURAL_WIND_ON"]
    elseif (streams[keyT["KEY_NATURAL_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["naturalWind"] = keyB["BYTE_NATURAL_WIND_OFF"]
    end
    --pmv
    if (streams[keyT["KEY_PMV"]] ~= nil ) then
		keyP["pmv"] = checkBoundary(streams[keyT["KEY_PMV"]], -3.5, 3)
    end

    --风速
    if (streams[keyT["KEY_FANSPEED"]] ~= nil) then
		keyP["fanspeedValue"] = checkBoundary(streams[keyT["KEY_FANSPEED"]], 1, 102)
    end

    --上下扫风
    if (streams[keyT["KEY_SWING_UD"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["swingUDValue"] = keyB["BYTE_SWING_UD_ON"]
    elseif (streams[keyT["KEY_SWING_UD"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["swingUDValue"] = keyB["BYTE_SWING_UD_OFF"]
    end
    
    --左右扫风
    if (streams[keyT["KEY_SWING_LR"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["swingLRValue"] = keyB["BYTE_SWING_LR_ON"]
    elseif (streams[keyT["KEY_SWING_LR"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["swingLRValue"] = keyB["BYTE_SWING_LR_OFF"]
    end
	
	--下左右扫风
    if (streams[keyT["KEY_SWING_LR_UNDER"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["swingLRUnderSwitch"] = keyB["BYTE_SWING_LR_UNDER_ENABLE"]
        keyP["swingLRValueUnder"] = keyB["BYTE_SWING_LR_UNDER_ON"]
    elseif (streams[keyT["KEY_SWING_LR_UNDER"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["swingLRUnderSwitch"] = keyB["BYTE_SWING_LR_UNDER_ENABLE"]
        keyP["swingLRValueUnder"] = keyB["BYTE_SWING_LR_UNDER_OFF"]
    end
    
    --定时开
    if (streams[keyT["KEY_TIME_ON"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_ON"]
    elseif (streams[keyT["KEY_TIME_ON"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_OFF"]
    end
    
    --定时关
    if (streams[keyT["KEY_TIME_OFF"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]
    elseif (streams[keyT["KEY_TIME_OFF"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]
    end
    
    --定时关机时间
    if (streams[keyT["KEY_CLOSE_TIME"]] ~= nil) then 
        keyP["closeTime"] = streams[keyT["KEY_CLOSE_TIME"]]
    end

    --定时开机时间
    if (streams[keyT["KEY_OPEN_TIME"]] ~= nil) then
		keyP["openTime"] = streams[keyT["KEY_OPEN_TIME"]]
    end
	
	--舒睡
	if (streams[keyT["KEY_COMFORT_SLEEP"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["comfortableSleepValue"] = keyB["BYTE_SLEEP_ON"]
		keyP["comfortableSleepSwitch"] = 0x40
		keyP["comfortableSleepTime"] = 0x0A
    elseif (streams[keyT["KEY_COMFORT_SLEEP"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["comfortableSleepValue"] = keyB["BYTE_SLEEP_OFF"]
		keyP["comfortableSleepSwitch"] = 0x00
		keyP["comfortableSleepTime"] = 0x00
    end
	
	--舒睡曲线
	if(streams[keyT["KEY_COMFORT_SLEEP_CURVE"]] ~= nil) then
	   streams[keyT["KEY_COMFORT_SLEEP_CURVE"]] = string.gsub(streams[keyT["KEY_COMFORT_SLEEP_CURVE"]],",","")
	   comfortByte = numstring2table(streams[keyT["KEY_COMFORT_SLEEP_CURVE"]])
	end
	   

    --电辅热
    if (streams[keyT["KEY_PTC"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["PTCValue"] = keyB["BYTE_PTC_ON"]
    elseif (streams[keyT["KEY_PTC"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["PTCValue"] = keyB["BYTE_PTC_OFF"]
    end
	
	--单独电辅热
	if (streams["independent_ptc"] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["independent_ptc"] = 0x08
    elseif (streams["independent_ptc"] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["independent_ptc"] = 0x00
    end
	

    --强劲
    if (streams[keyT["KEY_STRONG_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["strongWindValue"] = keyB["BYTE_STRONG_WIND_ON"]
    elseif (streams[keyT["KEY_STRONG_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["strongWindValue"] = keyB["BYTE_STRONG_WIND_OFF"]
    end

    --Turbo(AE、AB强劲)
    if (streams[keyT["KEY_TUBRO"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["tubroValue"] = 0x02
    elseif (streams[keyT["KEY_TUBRO"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["tubroValue"] = 0x00
    end

	--温度小数位(0/0.5)
	if(streams["small_temperature"] ~= nil) then
		keyP["smallTemperature"] = checkBoundary(streams["small_temperature"], 0, 0.5)
		if(keyP["smallTemperature"] == 0.5) then
			keyP["smallTemperature"] = 0x01
		else
			keyP["smallTemperature"] = 0x00
		end
	end

    --温度
    if (streams[keyT["KEY_TEMPERATURE"]] ~= nil) then
		if( type(streams[keyT["KEY_TEMPERATURE"]]) == "string") then
			streams[keyT["KEY_TEMPERATURE"]] = tonumber(streams[keyT["KEY_TEMPERATURE"]])
		end
		if(streams[keyT["KEY_TEMPERATURE"]] < 40) then
			keyP["temperature"] = checkBoundary(streams[keyT["KEY_TEMPERATURE"]], 10, 30)
		else
			keyP["temperature"] = checkBoundary(streams[keyT["KEY_TEMPERATURE"]], 50, 86)
			local temp = convert_to_C(keyP["temperature"]) 
			keyP["temperature"],keyP["smallTemperature"] = math.modf(temp)
			if(keyP["smallTemperature"] == 0.5) then
				keyP["smallTemperature"] = 0x01
			else
				keyP["smallTemperature"] = 0x00
			end
			keyP["temperature_unit"] = 1
		end
    end 
    
	--温度单位
    if (streams["temperature_unit"] ~= nil) then
        keyP["temperature_unit"] = checkBoundary(streams["temperature_unit"], 0, 1)
    end 
	
	--舒省
    if (streams[keyT["KEY_COMFORT_POWER_SAVE"]] == keyV["VALUE_FUNCTION_ON"]) then 
        keyP["comfortPowerSave"] = keyB["BYTE_COMFORT_POWER_SAVE_ON"]
    elseif (streams[keyT["KEY_COMFORT_POWER_SAVE"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["comfortPowerSave"] = keyB["BYTE_COMFORT_POWER_SAVE_OFF"]
    end
    
	--防过冷(快速降温，缓慢回温)
    if (jsonType == "control" and streams[keyT["KEY_PREVENT_SUPER_COOL"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_PREVENT_SUPER_COOL"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["prevent_super_cool"] = 0x01
		elseif (streams[keyT["KEY_PREVENT_SUPER_COOL"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["prevent_super_cool"] = 0x00
		end
	end 
	
	--8度制热
    if (streams[keyT["KEY_DEGREE8_HEAT"]] ~= nil) then 
		keyP["degree8_heat"] = checkBoundary(streams[keyT["KEY_DEGREE8_HEAT"]], 0, 1)
    end

	--防着凉
    if (streams[keyT["KEY_PREVENT_COLD"]] == keyV["VALUE_FUNCTION_ON"]) then 
        keyP["preventCold"] = 0x01
    elseif (streams[keyT["KEY_PREVENT_COLD"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["preventCold"] = 0x00
    end
	
	--防直吹
    if (jsonType == "control" and streams[keyT["KEY_PREVENT_STRAIGHT_WIND"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["prevent_straight_wind"] = checkBoundary(streams[keyT["KEY_PREVENT_STRAIGHT_WIND"]], 0, 2)
	end
	
	--fa系列防直吹
    if (jsonType == "control" and streams[keyT["KEY_FA_PREVENT_STRAIGHT_WIND"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["fa_prevent_straight_wind"] = checkBoundary(streams[keyT["KEY_FA_PREVENT_STRAIGHT_WIND"]], 0, 2)
	end

	--自动防直吹
    if (jsonType == "control" and streams[keyT["KEY_AUTO_PREVENT_STRAIGHT_WIND"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_AUTO_PREVENT_STRAIGHT_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["auto_prevent_straight_wind"] = 0x01
		elseif (streams[keyT["KEY_AUTO_PREVENT_STRAIGHT_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["auto_prevent_straight_wind"] = 0x00
		end 
	end

	--自清洁
    if (jsonType == "control" and streams[keyT["KEY_SELF_CLEAN"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_SELF_CLEAN"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["self_clean"] = 0x01
		elseif (streams[keyT["KEY_SELF_CLEAN"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["self_clean"] = 0x00
		end
	end 
	
	--风吹人
    if (jsonType == "control" and streams[keyT["KEY_WIND_STRAIGHT"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_WIND_STRAIGHT"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["wind_straight"] = 0x01
		elseif (streams[keyT["KEY_WIND_STRAIGHT"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["wind_straight"] = 0x00
		end
	end 
	
	--风避人
    if (jsonType == "control" and streams[keyT["KEY_WIND_AVOID"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_WIND_AVOID"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["wind_avoid"] = 0x01
		elseif (streams[keyT["KEY_WIND_AVOID"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["wind_avoid"] = 0x00
		end
	end 
	
	--yb系列风避人
    if (jsonType == "control" and streams[keyT["KEY_YB_WIND_AVOID"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_YB_WIND_AVOID"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["yb_wind_avoid"] = 0x02
		elseif (streams[keyT["KEY_YB_WIND_AVOID"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["yb_wind_avoid"] = 0x00
		end
	end 
	
	--智慧风
    if (jsonType == "control" and streams[keyT["KEY_INTELLIGENT_WIND"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_INTELLIGENT_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["intelligent_wind"] = 0x01
		elseif (streams[keyT["KEY_INTELLIGENT_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["intelligent_wind"] = 0x00
		end
	end 
	
	--无风感
    if (jsonType == "control" and  streams[keyT["KEY_NO_WIND_SENSE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["no_wind_sense"] = checkBoundary(streams[keyT["KEY_NO_WIND_SENSE"]], 0, 5)
	end
	
	--远近无风感
	if (jsonType == "control" and  streams[keyT["KEY_FA_NO_WIND_SENSE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["fa_no_wind_sense"] = checkBoundary(streams[keyT["KEY_FA_NO_WIND_SENSE"]], 1, 4)
	end
	
	--无风感等级
	if (streams["no_wind_sense_level"] ~= nil) then
	    keyP["no_wind_sense_level"] = streams["no_wind_sense_level"]
	end
	
	--儿童防冷风
    if (jsonType == "control" and streams[keyT["KEY_CHILD_PREVENT_COLD_WIND"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_CHILD_PREVENT_COLD_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["child_prevent_cold_wind"] = 0x01
		elseif (streams[keyT["KEY_CHILD_PREVENT_COLD_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["child_prevent_cold_wind"] = 0x00
		end
	end 
	
	--小天使
    if (jsonType == "control" and streams[keyT["KEY_LITTLE_ANGLE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_LITTLE_ANGLE"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["little_angel"] = 0x01
		elseif (streams[keyT["KEY_LITTLE_ANGLE"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["little_angel"] = 0x00
		end
	end 
	
	--冷热感
    if (jsonType == "control" and streams[keyT["KEY_COOL_HOT_SENSE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_COOL_HOT_SENSE"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["cool_hot_sense"] = 0x01
		elseif (streams[keyT["KEY_COOL_HOT_SENSE"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["cool_hot_sense"] = 0x00
		end
	end 
	
	--柔风感
    if (jsonType == "control" and streams[keyT["KEY_GENTLE_WIND_SENSE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_GENTLE_WIND_SENSE"]] == keyV["VALUE_FUNCTION_ON"]) then
		keyP["gentle_wind_sense"] = 0x03
		elseif (streams[keyT["KEY_GENTLE_WIND_SENSE"]] == keyV["VALUE_FUNCTION_OFF"]) then
		keyP["gentle_wind_sense"] = 0x01
		end
	end 
	
	--安防
	if (jsonType == "control" and streams[keyT["KEY_SECURITY"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_SECURITY"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["security"] = 0x01 
		elseif (streams[keyT["KEY_SECURITY"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["security"] = 0x00 
		end  
	end 
	--均匀风
	if (jsonType == "control" and streams[keyT["KEY_EVEN_WIND"]] ~= nil) then
        keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_EVEN_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["even_wind"] = 0x01 
		elseif (streams[keyT["KEY_EVEN_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["even_wind"] = 0x00 
		end  
	end
	--单风口
	if (jsonType == "control" and streams[keyT["KEY_SINGLE_TUYERE"]] ~= nil) then
        keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_SINGLE_TUYERE"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["single_tuyere"] = 0x01 
		elseif (streams[keyT["KEY_SINGLE_TUYERE"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["single_tuyere"] = 0x00 
		end  
	end
	--超远风
    if (jsonType == "control" and streams[keyT["KEY_EXTREME_WIND"]] ~= nil) then
        keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_EXTREME_WIND"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["extreme_wind"] = 0x01 
		elseif (streams[keyT["KEY_EXTREME_WIND"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["extreme_wind"] = 0x00 
		end  
	end
    --超远风等级
    if(streams["extreme_wind_level"] ~= nil) then
        keyP["extreme_wind_level"] = streams["extreme_wind_level"]
	end
	--语音控制
	if (jsonType == "control" and streams[keyT["KEY_VOICE_CONTROL"]] ~= nil) then
        keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_VOICE_CONTROL"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["voice_control"] = 0x03 
		elseif (streams[keyT["KEY_VOICE_CONTROL"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["voice_control"] = 0x00 
		end  
	end
	--预冷预热
	if (jsonType == "control" and streams[keyT["KEY_PRE_COOL_HOT"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_PRE_COOL_HOT"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["pre_cool_hot"] = 0x01 
		elseif (streams[keyT["KEY_PRE_COOL_HOT"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["pre_cool_hot"] = 0x00 
		end  
	end
	--水洗
	if (jsonType == "control" and streams[keyT["KEY_WATER_WASHING"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_WATER_WASHING"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["water_washing"] = 0x01 
		elseif (streams[keyT["KEY_WATER_WASHING"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["water_washing"] = 0x00 
		end         		
	end
	if(streams["water_washing_manual"] ~= nil) then
        keyP["water_washing_manual"] = streams["water_washing_manual"]
        keyP["water_washing_time"] = streams["water_washing_time"]
        keyP["water_washing_stage"] = streams["water_washing_stage"]	
	end
	--新风
	if (jsonType == "control" and streams[keyT["KEY_FRESH_AIR"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_FRESH_AIR"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["fresh_air"] = 0x01 
		elseif (streams[keyT["KEY_FRESH_AIR"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["fresh_air"] = 0x00 
		end 
        
        --keyP["fresh_air_fan_speed"] = streams["fresh_air_fan_speed"]
		--keyP["fresh_air_temp"] = streams["fresh_air_temp"]		
	end
	if(streams["fresh_air_fan_speed"] ~= nil) then
	   keyP["fresh_air_fan_speed"] = streams["fresh_air_fan_speed"]
	   keyP["fresh_air_temp"] = streams["fresh_air_temp"]
	end
    --家长控制
	if (jsonType == "control" and streams[keyT["KEY_PARENT_CONTROL"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_PARENT_CONTROL"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["parent_control"] = 0x01 
		elseif (streams[keyT["KEY_PARENT_CONTROL"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["parent_control"] = 0x00 
		end         		
	end
	if(streams["parent_control_temp_up"] ~= nil or keyP["parent_control_temp_down"] ~= nil) then
        keyP["parent_control_temp_up"] = streams["parent_control_temp_up"]
        keyP["parent_control_temp_down"] = streams["parent_control_temp_down"]		
	end
	
    --无人节能
	if (jsonType == "control" and streams[keyT["KEY_NOBODY_ENERGY_SAVE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_NOBODY_ENERGY_SAVE"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["nobody_energy_save"] = 0x01 
		elseif (streams[keyT["KEY_NOBODY_ENERGY_SAVE"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["nobody_energy_save"] = 0x00 
		end         		
	end
	
	--左右无风感
    if (jsonType == "control" and streams[keyT["KEY_PREVENT_STRAIGHT_WIND_LR"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["prevent_straight_wind_lr"] = checkBoundary(streams[keyT["KEY_PREVENT_STRAIGHT_WIND_LR"]], 0, 2)
	end
	
	--pm25值
    if (jsonType == "control" and streams[keyT["KEY_PM25_VALUE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["pm25_value"] = streams[keyT["KEY_PM25_VALUE"]]
	end
	
    --水泵开关
	if (jsonType == "control" and streams[keyT["KEY_WATER_PUMP"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_WATER_PUMP"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["water_pump"] = 0x01 
		elseif (streams[keyT["KEY_WATER_PUMP"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["water_pump"] = 0x00 
		end         		
	end
	
	--上下摆风角度
	if (jsonType == "control" and streams[keyT["KEY_WIND_SWING_UD_ANGLE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["wind_swing_ud_angle"] = streams["wind_swing_ud_angle"]        		
	end
	--左右摆风角度
	if (jsonType == "control" and streams[keyT["KEY_WIND_SWING_LR_ANGLE"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["wind_swing_lr_angle"] = streams["wind_swing_lr_angle"]        		
	end
    --智能功能总开关
	if (jsonType == "control" and streams[keyT["KEY_INTENLLIGENT_CONTROL"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams[keyT["KEY_INTENLLIGENT_CONTROL"]] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["intelligent_control"] = 0x01 
		elseif (streams[keyT["KEY_INTENLLIGENT_CONTROL"]] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["intelligent_control"] = 0x00 
		end         		
	end
	
	--音量控制
    if (jsonType == "control" and streams[keyT["KEY_VOLUME_CONTROL"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["volume_control"] = checkBoundary(streams[keyT["KEY_VOLUME_CONTROL"]], 0, 100)
	end
	
	--语音开关
    if (jsonType == "control" and streams[keyT["KEY_VOICE_CONTROL_NEW"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["voice_control_new"] = checkBoundary(streams[keyT["KEY_VOICE_CONTROL_NEW"]], 0, 3)
	end
	--温度上下限
	if (jsonType == "control" and (streams[keyT["KEY_AUTO_TEMP_UP"]] ~= nil or streams[keyT["KEY_AUTO_TEMP_DOWN"]] ~= nil or streams[keyT["KEY_COOL_TEMP_UP"]] ~= nil or streams[keyT["KEY_COOL_TEMP_DOWN"]] ~= nil or streams[keyT["KEY_HEAT_TEMP_UP"]] ~= nil or streams[keyT["KEY_HEAT_TEMP_DOWN"]] ~= nil)) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
	end
	if(streams["cool_temp_up"] ~= nil) then 
	    keyP["cool_temp_up"] = streams["cool_temp_up"]
	end
	if(streams["cool_temp_down"] ~= nil) then 
	    keyP["cool_temp_down"] = streams["cool_temp_down"]
	end
	if(streams["auto_temp_up"] ~= nil) then 
	    keyP["auto_temp_up"] = streams["auto_temp_up"]
	end
	if(streams["auto_temp_down"] ~= nil) then 
	    keyP["auto_temp_down"] = streams["auto_temp_down"]
	end
	if(streams["heat_temp_up"] ~= nil) then 
	    keyP["heat_temp_up"] = streams["heat_temp_up"]
	end
	if(streams["heat_temp_down"] ~= nil) then 
	    keyP["heat_temp_down"] = streams["heat_temp_down"]
	end
	--省电
    if (streams[keyT["KEY_POWER_SAVING"]] == keyV["VALUE_FUNCTION_ON"]) then
        keyP["power_saving"] = 0x08
    elseif (streams[keyT["KEY_POWER_SAVING"]] == keyV["VALUE_FUNCTION_OFF"]) then
        keyP["power_saving"] = 0x00
    end
	--遥控器锁定
	if (jsonType == "control" and streams[keyT["KEY_REMOTE_CONTROL_LOCK"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["remote_control_lock"] = streams["remote_control_lock"]
		keyP["remote_control_lock_control"] = streams["remote_control_lock_control"]   		
	end
	--空调允许运行时间
	if (jsonType == "control" and streams[keyT["KEY_OPERATING_TIME"]] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["operating_time"] = streams["operating_time"] 		
	end
	
	--Gear
	if (jsonType == "control" and streams["rate_select"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["rate_select"] = streams["rate_select"] 		
	end
	
	--Jet Cool
	if (jsonType == "control" and streams["jet_cool"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		--keyP["jet_cool"] = streams["jet_cool"]
		if (streams["jet_cool"] == keyV["VALUE_FUNCTION_ON"]) then 
			keyP["jet_cool"] = 0x01 
		elseif (streams["jet_cool"] == keyV["VALUE_FUNCTION_OFF"]) then 
			keyP["jet_cool"] = 0x00 
		end 		
	end
	
	--四向风
	if (streams["main_horizontal_guide_strip_1"] ~= nil) then 
		keyP["main_horizontal_guide_strip_1"] = streams["main_horizontal_guide_strip_1"] 		
	end
	if (streams["main_horizontal_guide_strip_2"] ~= nil) then 
		keyP["main_horizontal_guide_strip_2"] = streams["main_horizontal_guide_strip_2"] 		
	end
	if (streams["main_horizontal_guide_strip_3"] ~= nil) then 
		keyP["main_horizontal_guide_strip_3"] = streams["main_horizontal_guide_strip_3"] 		
	end
	if (streams["main_horizontal_guide_strip_4"] ~= nil) then 
		keyP["main_horizontal_guide_strip_4"] = streams["main_horizontal_guide_strip_4"] 		
	end
	if (jsonType == "control" and (streams["main_horizontal_guide_strip_1"] ~= nil or streams["main_horizontal_guide_strip_2"] ~= nil or streams["main_horizontal_guide_strip_3"] ~= nil or streams["main_horizontal_guide_strip_4"] ~= nil)) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["main_strip_control"] = 1
	end
	
	
	if (streams["sup_horizontal_guide_strip_1"] ~= nil) then 
		keyP["sup_horizontal_guide_strip_1"] = streams["sup_horizontal_guide_strip_1"] 		
	end
	if (streams["sup_horizontal_guide_strip_2"] ~= nil) then 
		keyP["sup_horizontal_guide_strip_2"] = streams["sup_horizontal_guide_strip_2"] 		
	end
	if (streams["sup_horizontal_guide_strip_3"] ~= nil) then 
		keyP["sup_horizontal_guide_strip_3"] = streams["sup_horizontal_guide_strip_3"] 		
	end
	if (streams["sup_horizontal_guide_strip_4"] ~= nil) then 
		keyP["sup_horizontal_guide_strip_4"] = streams["sup_horizontal_guide_strip_4"] 		
	end
	if (jsonType == "control" and (streams["sup_horizontal_guide_strip_1"] ~= nil or streams["sup_horizontal_guide_strip_2"] ~= nil or streams["sup_horizontal_guide_strip_3"] ~= nil or streams["sup_horizontal_guide_strip_4"] ~= nil)) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1		
		keyP["sup_strip_control"] = 1
	end
	
	--声音(外销蜂鸣器)
	if (jsonType == "control" and streams["sound"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["sound"] = streams["sound"] 		
	end
	
	
	--新风滤网运行时间清零
    if (streams[keyT["KEY_FRESH_FILTER_RESET"]] ~= nil) then
		if(tonumber(streams[keyT["KEY_FRESH_FILTER_RESET"]]) == 0x01)then
			keyP["fresh_filter_reset"] = 0x08
		end
    end
	
	--普通滤网运行时间清零
    if (streams[keyT["KEY_COMMON_FILTER_RESET"]] ~= nil) then
		if(tonumber(streams[keyT["KEY_COMMON_FILTER_RESET"]]) == 0x01)then
			keyP["common_filter_reset"] = 0x80
		end
    end
	
	--FA防直吹判断
	if (streams["prevent_straight_wind_flag"] ~= nil) then
		keyP["prevent_straight_wind_flag"] = streams["prevent_straight_wind_flag"]
    end
	
	if (streams["sleep_status"] ~= nil) then
		keyP["sleep_status"] = streams["sleep_status"]
    end
	
	--负离子
	if (jsonType == "control" and streams["anion"] ~= nil) then 
		--keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["anion"] = streams["anion"] 		
	end
	
	--ieco
	
	if (jsonType == "control" and (streams["ieco_target_rate"] ~= nil or streams["ieco_indoor_wind_speed"] ~= nil or streams["ieco_outdoor_wind_speed"] ~= nil or streams["ieco_frame"] ~= nil or streams["ieco_expansion_valve"] ~= nil or streams["ieco_switch"] ~= nil)) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["ieco_status"] = 1
	end				  
	if (streams["ieco_switch"] ~= nil) then 
		keyP["ieco_switch"] = streams["ieco_switch"]
	end
	if (streams["ieco_target_rate"] ~= nil) then 
		keyP["ieco_target_rate"] = streams["ieco_target_rate"]
	end
	if (streams["ieco_indoor_wind_speed"] ~= nil) then 
		keyP["ieco_indoor_wind_speed"] = streams["ieco_indoor_wind_speed"]
	end
	if (streams["ieco_outdoor_wind_speed"] ~= nil) then 
		keyP["ieco_outdoor_wind_speed"] = streams["ieco_outdoor_wind_speed"]
	end
	if (streams["ieco_frame"] ~= nil) then 
		keyP["ieco_frame"] = streams["ieco_frame"]
	end
	if (streams["ieco_expansion_valve"] ~= nil) then 
		keyP["ieco_expansion_valve"] = streams["ieco_expansion_valve"]
	end
	if (streams["ieco_indoor_wind_speed_level"] ~= nil) then 
		keyP["ieco_indoor_wind_speed_level"] = streams["ieco_indoor_wind_speed_level"]
	end
	if (streams["ieco_outdoor_wind_speed_level"] ~= nil) then 
		keyP["ieco_outdoor_wind_speed_level"] = streams["ieco_outdoor_wind_speed_level"]
	end
	if (streams["ieco_number"] ~= nil) then 
		keyP["ieco_number"] = streams["ieco_number"]
	end
	--环绕风 
	if (jsonType == "control" and streams["wind_around"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		if (streams["wind_around"] == keyV["VALUE_FUNCTION_ON"]) then
			keyP["wind_around"] = 0x01
		elseif (streams["wind_around"] == keyV["VALUE_FUNCTION_OFF"]) then
			keyP["wind_around"] = 0x00
		end
	end
    if (streams["wind_around_ud"] ~= nil) then
		keyP["wind_around_ud"] = streams["wind_around_ud"]
    end
	--mito
	if (streams["mito_cool"] ~= nil) then
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["mito_cool"] = streams["mito_cool"]
    end
	if (streams["mito_heat"] ~= nil) then
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["mito_heat"] = streams["mito_heat"]
    end
	if (streams["dr_time_hour"] ~= nil) then
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["dr_time_hour"] = streams["dr_time_hour"]
    end
	if (streams["dr_time_min"] ~= nil) then
		keyP["dr_time_min"] = streams["dr_time_min"]
    end
	--防直吹类型选择
	if (jsonType == "control" and streams["prevent_straight_wind_select"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["prevent_straight_wind_select"] = streams["prevent_straight_wind_select"]
    end
	--CVP
	if (jsonType == "control" and streams["cvp"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["cvp"] = streams["cvp"]
    end
	
	--CB1新风感
	if (jsonType == "control" and streams["new_wind_sense"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["new_wind_sense"] = streams["new_wind_sense"]
    end
	--COMFORT
	if (jsonType == "control" and streams["comfort"] ~= nil) then 
		keyP["propertyNumber"] = keyP["propertyNumber"] + 1
		keyP["comfort"] = streams["comfort"]
    end
	
	
	
	--status中的属性协议不组，只组control中的属性协议
	if(jsonType == "status") then
		keyP["propertyNumber"] = 0
	end
end

--根据 bin 修改属性变量
local function  binToModel(binData,deviceSN8)

    local messageBytes = binData
    if ((dataType==0x02 and messageBytes[0] == 0xC0)or (dataType==0x03 and messageBytes[0] == 0xC0) or (dataType==0x05 and messageBytes[0] == 0xA0)) then
		if(#binData < 19) then
			return nil
		end
		keyP["powerValue"] = bit.band(messageBytes[1], 0x01)
        keyP["modeValue"] = bit.band(messageBytes[2], 0xE0)
		
		if(keyP["modeValue"] == keyB["BYTE_MODE_SMART_DRY"] or keyP["modeValue"] == keyB["BYTE_MODE_DRY"]) then
			if(dataType == 0x05) then
				keyP["smartDryValue"] = bit.band(messageBytes[13], 0x7F)
			else
				keyP["smartDryValue"] = bit.band(messageBytes[19], 0x7F)
			end
		end
		
        if(dataType == 0x05) then
            --CA机型 11447、11451、11453、11455、11457、11459、11525、11527、11533、11535
            if deviceSN8=="11447" or deviceSN8=="11451" or deviceSN8=="11453" or deviceSN8=="11455" or deviceSN8=="11457" or deviceSN8=="11459" or deviceSN8=="11525" or deviceSN8=="11527" or deviceSN8=="11533" or deviceSN8=="11535" then
                keyP["temperature"] = bit.rshift(bit.band(messageBytes[1], 0x7C), 2) + 0x0C
                keyP["smallTemperature"] = bit.rshift(bit.band(messageBytes[1], 0x02), 1)
            else
                keyP["temperature"] = bit.rshift(bit.band(messageBytes[1], 0x3E), 1) + 0x0C
                keyP["smallTemperature"] = bit.rshift(bit.band(messageBytes[1], 0x40), 6)
            end
        else
            keyP["temperature"] = bit.band(messageBytes[2], 0x0F) + 0x10
			keyP["smallTemperature"] = bit.rshift(bit.band(messageBytes[2], 0x10),4)
        end
        
        keyP["fanspeedValue"] = bit.band(messageBytes[3], 0x7F)

        if (bit.band(messageBytes[4], keyB["BYTE_START_TIMER_SWITCH_ON"]) == keyB["BYTE_START_TIMER_SWITCH_ON"]) then
            keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_ON"]
        else
           keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_OFF"]
        end

        if (bit.band(messageBytes[5], keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]) == keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]) then
            keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]
        else
            keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]
        end

        keyP["closeHour"] = bit.rshift(bit.band(messageBytes[5], 0x7F), 2)
		
        keyP["closeStepMintues"] = bit.band(messageBytes[5], 0x03)
		
        keyP["closeMin"] = 15 - bit.band(messageBytes[6], 0x0f)
		
        keyP["closeTime"] = keyP["closeHour"] * 60 + keyP["closeStepMintues"] * 15 + keyP["closeMin"]

        keyP["openHour"] = bit.rshift(bit.band(messageBytes[4], 0x7F), 2)
		
        keyP["openStepMintues"] = bit.band(messageBytes[4], 0x03)
		
        keyP["openMin"] = 15 - bit.rshift(bit.band(messageBytes[6], 0xf0),4)
		
        keyP["openTime"] = keyP["openHour"] * 60 + keyP["openStepMintues"] * 15 + keyP["openMin"]

        keyP["strongWindValue"] = bit.band(messageBytes[8], 0x20)
        keyP["tubroValue"] = bit.band(messageBytes[10], 0x02)
		keyP["sleep_status"] = bit.band(messageBytes[10], 0x01)
		
		keyP["power_saving"] = bit.band(messageBytes[8], 0x08)
		
        keyP["comfortableSleepValue"] = bit.band(messageBytes[8], 0x03)
		
		keyP["comfortableSleepSwitch"] = bit.band(messageBytes[9], 0x40)
		
        keyP["pmv"] = bit.band(messageBytes[14], 0x0f) * 0.5 - 3.5
		keyP["comfortableSleepTime"] = bit.band(messageBytes[17], 0x15)
        keyP["naturalWind"] = bit.band(messageBytes[9], 0x02)
		
        keyP["PTCValue"] = bit.band(messageBytes[9], 0x18)
		
        keyP["purifierValue"] = bit.band(messageBytes[9], 0x20)
		
        keyP["ecoValue"] = bit.lshift(bit.band(messageBytes[9], 0x10), 3)
		
        keyP["dryValue"] = bit.band(messageBytes[9], 0x04)
				
        keyP["swingLRValue"] = bit.band(messageBytes[7], 0x03)
		
        keyP["swingUDValue"] = bit.band(messageBytes[7], 0x0C)
		
		keyP["fresh_filter_timeout_ae2"] = bit.rshift(bit.band(messageBytes[13], 0x20),5)
		
		if(#binData >= 21) then
			keyP["swingLRUnderSwitch"] = bit.band(messageBytes[19], 0x80)
			keyP["swingLRValueUnder"] = bit.band(messageBytes[20], 0x80)
		end

        if(dataType == 0x02 or dataType == 0x03) then
            --if ((messageBytes[11] ~= 0) and (messageBytes[11] ~= 0xFF)) then
                keyP["indoorTemperatureValue"] = (messageBytes[11] - 50) / 2
                keyP["smallIndoorTemperatureValue"]=bit.band(messageBytes[15],0xF);
            --end

            --if ((messageBytes[12] ~= 0) and (messageBytes[12] ~= 0xFF)) then
                keyP["outdoorTemperatureValue"]  = (messageBytes[12] - 50) / 2
                keyP["smallOutdoorTemperatureValue"]=bit.rshift(messageBytes[15],4);
				keyP["fresh_filter_timeout"] = bit.rshift(bit.band(messageBytes[13], 0x40),6)
            --end
        end
		if(dataType ~= 0x05) then
			keyP["errorCode"]=messageBytes[16]
		end
		
		--温度单位
		if(dataType == 0x05) then
			keyP["temperature_unit"] = bit.rshift(bit.band(messageBytes[9], 0x80),7)
		else
			keyP["temperature_unit"] = bit.rshift(bit.band(messageBytes[10], 0x04),2)
		end
		
		
		--防着凉
		keyP["preventCold"] = bit.rshift(bit.band(messageBytes[10], 0x20),5)

		--温度(低于17度)
		if(dataType == 0x05) then
			local temp = bit.rshift(bit.band(messageBytes[12], 0x3E),1)
			if (temp > 0 and temp <= 25)  then 
				keyP["temperature"] = temp + 12
			elseif(temp == 0)then
			else
				keyP["temperature"] = temp - 19
			end
		else
			local temp = bit.band(messageBytes[13], 0x1F)
			if (temp > 0 and temp <= 25)  then 
				keyP["temperature"] = temp + 12 
			elseif(temp == 0)then
			else
				keyP["temperature"] = temp - 19
			end
		end

		--屏显状态
		keyP["screenDisplayNowValue"] = bit.rshift(bit.band(messageBytes[14], 0x70),4)
		--舒省
		if (messageBytes[0] == 0xA0) then
			keyP["comfortPowerSave"] = bit.band(messageBytes[14], 0x01)
		else
			if(#binData >= 24) then
				keyP["comfortPowerSave"] = bit.band(messageBytes[22], 0x01)
			end
			
		end
		
		if(#binData >= 23) then
			--8度制热
			keyP["degree8_heat"] = bit.rshift(bit.band(messageBytes[21], 0x80),7)
		end
		
		--单独电辅热
		if(dataType == 0x05) then
			keyP["independent_ptc"] = bit.rshift(bit.band(messageBytes[11], 0x08),3)
		else
			keyP["independent_ptc"] = bit.rshift(bit.band(messageBytes[8], 0x40),6)
		end

		--新风滤网
		if(#binData >= 29) then
			keyP["fresh_filter_time_total"] = messageBytes[25] * 256 + messageBytes[24] 
			keyP["fresh_filter_time_use"] = messageBytes[27] * 256 + messageBytes[26] 
		end
    end
    if ((dataType==0x04 and messageBytes[0] == 0xA1)) then
        --本次开机运行时间
		keyP["currentWorkTime"] = bit.bor((bit.band(bit.lshift(messageBytes[9],8), 0xFF00)),(bit.band(messageBytes[10], 0x00FF))) * 60 * 24 + messageBytes[11] * 60 + messageBytes[12] 
		--计算方式由空调事业部黑继伟提供
        if (messageBytes[13]~=0x00 and messageBytes[13]~=0xff) then
            keyP["indoorTemperatureValue"] = (messageBytes[13]-50)/2
            keyP["smallIndoorTemperatureValue"]=bit.band(messageBytes[18],0xF);
        end
        if (messageBytes[14]~=0x00 and messageBytes[14]~=0xff) then
            keyP["outdoorTemperatureValue"]  = (messageBytes[14]-50)/2
            keyP["smallOutdoorTemperatureValue"]=bit.rshift(messageBytes[18],4);
        end
    end
	
	if ((dataType==0xA0 and messageBytes[0] == 0x00)) then
		keyP["product_type"] = messageBytes[2]
		keyP["machine_type"] = messageBytes[3]
	end
	

    if ((dataType==0x02 and messageBytes[0] == 0xB0)  or (dataType==0x03 and messageBytes[0] == 0xB1)) then
        --新协议，变长属性协议
		if(#binData < 8) then
			return nil
		end
		keyP["propertyNumber"] = messageBytes[1]
		local cursor = 2
        for i = 1,  keyP["propertyNumber"] do
			if (messageBytes[cursor + 0] == 0x49 and messageBytes[cursor + 1] == 0x00) then
				keyP["prevent_super_cool"] = messageBytes[cursor + 4]
				cursor = cursor + 9
			end
			if (messageBytes[cursor + 0] == 0x42 and messageBytes[cursor + 1] == 0x00) then
				if(messageBytes[cursor + 4] ~= nil)then
					keyP["prevent_straight_wind"] = messageBytes[cursor + 4]
				end
				print(keyP["prevent_straight_wind"])
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x26 and messageBytes[cursor + 1] == 0x02) then
				keyP["auto_prevent_straight_wind"] = messageBytes[cursor + 4]	
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x39 and messageBytes[cursor + 1] == 0x00) then
				keyP["self_clean"] = messageBytes[cursor + 4]	
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x32 and messageBytes[cursor + 1] == 0x00) then
			    if (messageBytes[cursor + 4] == 0x01) then 
					keyP["wind_straight"] = 0x01
				end
				if (messageBytes[cursor + 4] == 0x02) then 
					keyP["wind_avoid"] = 0x01
					keyP["yb_wind_avoid"] = 0x02
				end	
				if (messageBytes[cursor + 4] == 0x00) then 
					keyP["wind_straight"] = 0x00
					keyP["wind_avoid"] = 0x00
                    keyP["yb_wind_avoid"] = 0x00					
				end
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x33 and messageBytes[cursor + 1] == 0x00) then
				keyP["wind_avoid"] = messageBytes[cursor + 4]	
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x34 and messageBytes[cursor + 1] == 0x00) then
				keyP["intelligent_wind"] = messageBytes[cursor + 4]	
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x3A and messageBytes[cursor + 1] == 0x00) then
				keyP["child_prevent_cold_wind"] = messageBytes[cursor + 4]	
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x18 and messageBytes[cursor + 1] == 0x00) then			
				if (messageBytes[cursor + 3] == 0x02) then
					keyP["no_wind_sense_level"] = messageBytes[cursor + 5]
				    cursor = cursor + 6
				else
				    keyP["no_wind_sense"] = messageBytes[cursor + 4]
				    cursor = cursor + 5
				end
			end
			if (messageBytes[cursor + 0] == 0x1B and messageBytes[cursor + 1] == 0x02) then
				keyP["little_angel"] = messageBytes[cursor + 4]	
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x21 and messageBytes[cursor + 1] == 0x00) then
				keyP["cool_hot_sense"] = messageBytes[cursor + 4]	
				cursor = cursor + 12
			end
			if (messageBytes[cursor + 0] == 0x29 and messageBytes[cursor + 1] == 0x00) then
				keyP["security"] = messageBytes[cursor + 4]
				if(messageBytes[cursor + 4] == 2) then
					keyP["security"] = 0
				end
				if(messageBytes[cursor + 4] == 3) then
					keyP["security"] = 1
				end
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x4E and messageBytes[cursor + 1] == 0x00) then
				keyP["even_wind"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x4F and messageBytes[cursor + 1] == 0x00) then
				keyP["single_tuyere"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x4C and messageBytes[cursor + 1] == 0x00) then
				keyP["extreme_wind"] = messageBytes[cursor + 4]
				keyP["extreme_wind_level"] = messageBytes[cursor + 5]
				cursor = cursor + 6
			end
			if (messageBytes[cursor + 0] == 0x20 and messageBytes[cursor + 1] == 0x00) then
				keyP["voice_control"] = messageBytes[cursor + 4]
				keyP["voice_control_new"] = messageBytes[cursor + 4]
				cursor = cursor + 24
			end
			if (messageBytes[cursor + 0] == 0x01 and messageBytes[cursor + 1] == 0x02) then
				keyP["pre_cool_hot"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x4A and messageBytes[cursor + 1] == 0x00) then
				keyP["water_washing_manual"] = messageBytes[cursor + 4]
				keyP["water_washing"] = messageBytes[cursor + 5]
				keyP["water_washing_time"] = messageBytes[cursor + 6]
				keyP["water_washing_stage"] = messageBytes[cursor + 7]
				cursor = cursor + 8
			end
			if (messageBytes[cursor + 0] == 0x4B and messageBytes[cursor + 1] == 0x00) then
				keyP["fresh_air"] = messageBytes[cursor + 4]
				keyP["fresh_air_fan_speed"] = messageBytes[cursor + 5]
				keyP["fresh_air_temp"] = messageBytes[cursor + 6]
				cursor = cursor + 7
			end
			if (messageBytes[cursor + 0] == 0x51 and messageBytes[cursor + 1] == 0x00) then
				keyP["parent_control"] = messageBytes[cursor + 4]
				keyP["parent_control_temp_up"] = messageBytes[cursor + 5]
				keyP["parent_control_temp_down"] = messageBytes[cursor + 6]
				cursor = cursor + 9
			end
			if (messageBytes[cursor + 0] == 0x43 and messageBytes[cursor + 1] == 0x00) then
				if (messageBytes[cursor + 4] == 0x01 or messageBytes[cursor + 4] == 0x00) then 
					keyP["gentle_wind_sense"] = 0x01
					keyP["prevent_straight_wind"] = 0x01
					keyP["fa_no_wind_sense"] = 0x01
				end
				if (messageBytes[cursor + 4] == 0x02) then 
					keyP["gentle_wind_sense"] = 0x01
					keyP["prevent_straight_wind"] = 0x02
					keyP["fa_no_wind_sense"] = 0x02
				end
				if (messageBytes[cursor + 4] == 0x03) then 
					keyP["gentle_wind_sense"] = 0x03
					keyP["prevent_straight_wind"] = 0x01
					keyP["fa_no_wind_sense"] = 0x03
				end
				if (messageBytes[cursor + 4] == 0x04) then 
					keyP["prevent_straight_wind"] = 0x01
					keyP["gentle_wind_sense"] = 0x01
					keyP["fa_no_wind_sense"] = 0x04
				end
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x30 and messageBytes[cursor + 1] == 0x00) then
				keyP["nobody_energy_save"] = messageBytes[cursor + 4]
				cursor = cursor + 10
			end
			if (messageBytes[cursor + 0] == 0x09 and messageBytes[cursor + 1] == 0x04) then
				keyP["filter_level"] = messageBytes[cursor + 5]
				keyP["filter_value"] = messageBytes[cursor + 14]
				cursor = cursor + 17
			end
			if (messageBytes[cursor + 0] == 0x58 and messageBytes[cursor + 1] == 0x00) then
				keyP["prevent_straight_wind_lr"] = messageBytes[cursor + 4]
				keyP["prevent_straight_wind_select"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x0B and messageBytes[cursor + 1] == 0x02) then
				keyP["pm25_value"] = messageBytes[cursor + 6] * 256 + messageBytes[cursor + 5]
				cursor = cursor + 7
			end
			if (messageBytes[cursor + 0] == 0x50 and messageBytes[cursor + 1] == 0x00) then
				keyP["water_pump"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x31 and messageBytes[cursor + 1] == 0x00) then
				keyP["intelligent_control"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x24 and messageBytes[cursor + 1] == 0x00) then
				keyP["volume_control"] = messageBytes[cursor + 5]
				cursor = cursor + 8
			end
			if (messageBytes[cursor + 0] == 0x09 and messageBytes[cursor + 1] == 0x00) then
				keyP["wind_swing_ud_angle"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x0A and messageBytes[cursor + 1] == 0x00) then
				keyP["wind_swing_lr_angle"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x44 and messageBytes[cursor + 1] == 0x00) then
				keyP["face_register"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x1A and messageBytes[cursor + 1] == 0x00) then
				if(messageBytes[cursor + 3] == 0x00)then
					cursor = cursor + 4
				else
					cursor = cursor + 5
				end
				
			end
			if (messageBytes[cursor + 0] == 0x25 and messageBytes[cursor + 1] == 0x02) then
				keyP["cool_temp_down"] = messageBytes[cursor + 4]
				keyP["cool_temp_up"] = messageBytes[cursor + 5]
				keyP["auto_temp_down"] = messageBytes[cursor + 6]
				keyP["auto_temp_up"] = messageBytes[cursor + 7]
				keyP["heat_temp_down"] = messageBytes[cursor + 8]
				keyP["heat_temp_up"] = messageBytes[cursor + 9]
				cursor = cursor + 11
			end
			if (messageBytes[cursor + 0] == 0x27 and messageBytes[cursor + 1] == 0x02) then
				keyP["remote_control_lock"] = messageBytes[cursor + 4]
				keyP["remote_control_lock_control"] = messageBytes[cursor + 5]
				cursor = cursor + 6
			end
			if (messageBytes[cursor + 0] == 0x28 and messageBytes[cursor + 1] == 0x02) then
				keyP["operating_time"] = bit.bor(messageBytes[cursor + 4],bit.bor(bit.lshift(messageBytes[cursor + 5],8),bit.lshift(messageBytes[cursor + 6], 16)))
				cursor = cursor + 7
			end
			if (messageBytes[cursor + 0] == 0x15 and messageBytes[cursor + 1] == 0x00) then
				keyP["indoor_humidity"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x67 and messageBytes[cursor + 1] == 0x00) then
				keyP["jet_cool"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x34 and messageBytes[cursor + 1] == 0x02) then
				keyP["body_check"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x48 and messageBytes[cursor + 1] == 0x00) then
				keyP["rate_select"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x30 and messageBytes[cursor + 1] == 0x02) then
				keyP["main_horizontal_guide_strip_1"] =  messageBytes[cursor + 4]
				keyP["main_horizontal_guide_strip_2"] =  messageBytes[cursor + 5]
				keyP["main_horizontal_guide_strip_3"] =  messageBytes[cursor + 6]
				keyP["main_horizontal_guide_strip_4"] =  messageBytes[cursor + 7]
				cursor = cursor + 8
			end
			if (messageBytes[cursor + 0] == 0x31 and messageBytes[cursor + 1] == 0x02) then
				keyP["sup_horizontal_guide_strip_1"] =  messageBytes[cursor + 4]
				keyP["sup_horizontal_guide_strip_2"] =  messageBytes[cursor + 5]
				keyP["sup_horizontal_guide_strip_3"] =  messageBytes[cursor + 6]
				keyP["sup_horizontal_guide_strip_4"] =  messageBytes[cursor + 7]
				cursor = cursor + 8
			end
			if (messageBytes[cursor + 0] == 0x32 and messageBytes[cursor + 1] == 0x02) then
				keyP["twins_machine"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x33 and messageBytes[cursor + 1] == 0x02) then
				if(messageBytes[cursor + 3] == 0x00)then
					cursor = cursor + 4
				else
					keyP["guide_strip_type"] =  messageBytes[cursor + 4]
					cursor = cursor + 5
				end
			end
			if (messageBytes[cursor + 0] == 0x2C and messageBytes[cursor + 1] == 0x02) then
				keyP["sound"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x1E and messageBytes[cursor + 1] == 0x02) then
				keyP["anion"] =  messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0xE3 and messageBytes[cursor + 1] == 0x00) then
				keyP["ieco_number"] =  messageBytes[cursor + 4]
				keyP["ieco_switch"] =  messageBytes[cursor + 5]
				--keyP["ieco_target_rate"] =  messageBytes[cursor + 6]
				--keyP["ieco_indoor_wind_speed"] =  messageBytes[cursor + 7]
				--keyP["ieco_outdoor_wind_speed"] =  bit.bor(messageBytes[cursor + 8],bit.bor(bit.lshift(messageBytes[cursor + 9],8)))
				--keyP["ieco_expansion_valve"] =  messageBytes[cursor + 10]
				cursor = cursor + 6
			end
			if (messageBytes[cursor + 0] == 0xE0 and messageBytes[cursor + 1] == 0x00) then
				keyP["ieco_frame"] =  messageBytes[cursor + 4]
				keyP["ieco_target_rate"] = bit.bor(messageBytes[cursor + 5],bit.bor(bit.lshift(messageBytes[cursor + 6],8)))
				keyP["ieco_indoor_wind_speed_level"] =  messageBytes[cursor + 7]
				keyP["ieco_indoor_wind_speed"] =  bit.bor(messageBytes[cursor + 8],bit.bor(bit.lshift(messageBytes[cursor + 9],8)))
				keyP["ieco_outdoor_wind_speed_level"] =  messageBytes[cursor + 10]
				keyP["ieco_outdoor_wind_speed"] =  bit.bor(messageBytes[cursor + 11],bit.bor(bit.lshift(messageBytes[cursor + 12],8)))
				keyP["ieco_expansion_valve"] =  messageBytes[cursor + 13]
				cursor = cursor + 14
			end
			if (messageBytes[cursor + 0] == 0x59 and messageBytes[cursor + 1] == 0x00) then
				keyP["wind_around"] = messageBytes[cursor + 4]
				keyP["wind_around_ud"] = messageBytes[cursor + 5]
				cursor = cursor + 6
			end
			if (messageBytes[cursor + 0] == 0x8D and messageBytes[cursor + 1] == 0x00) then
				keyP["mito_cool"] =  (messageBytes[cursor + 4] - 50) / 2
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x8E and messageBytes[cursor + 1] == 0x00) then
				keyP["mito_heat"] =  (messageBytes[cursor + 4] - 50) / 2
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x8F and messageBytes[cursor + 1] == 0x00) then
				keyP["dr_time_min"] = messageBytes[cursor + 4]
				keyP["dr_time_hour"] = messageBytes[cursor + 5]
				cursor = cursor + 6
			end
			if (messageBytes[cursor + 0] == 0x90 and messageBytes[cursor + 1] == 0x00) then
				keyP["has_cool_heat_amount"] = messageBytes[cursor + 4]
				keyP["t2_heat"] = messageBytes[cursor + 5]
				keyP["tp_heat"] = messageBytes[cursor + 6]
				keyP["k1_value"] = messageBytes[cursor + 7]
				keyP["k2_value"] = messageBytes[cursor + 8]
				keyP["k3_value"] = messageBytes[cursor + 9]
				keyP["k4_value"] = messageBytes[cursor + 10]
				keyP["cool_strong_wind_speed"] = bit.bor(messageBytes[cursor + 11],bit.bor(bit.lshift(messageBytes[cursor + 12],8)))
				keyP["cool_strong_wind_amount"] = bit.bor(messageBytes[cursor + 13],bit.bor(bit.lshift(messageBytes[cursor + 14],8)))
				cursor = cursor + 15
			end
			if (messageBytes[cursor + 0] == 0x91 and messageBytes[cursor + 1] == 0x00) then
				keyP["has_icheck"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x98 and messageBytes[cursor + 1] == 0x00) then
				keyP["cvp"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0xAA and messageBytes[cursor + 1] == 0x00) then
				keyP["new_wind_sense"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0xAD and messageBytes[cursor + 1] == 0x00) then
				keyP["comfort"] = messageBytes[cursor + 4]
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0xAB and messageBytes[cursor + 1] == 0x00) then
				if(messageBytes[cursor + 2] == 0x11 or messageBytes[cursor + 2] == 0x10 or messageBytes[cursor + 3] == 0x00) then
					cursor = cursor + 4
				else
					local inCodeString = ""
					cursor = cursor + 12
					for i= 0,15 do 
						local stringChar = string.char(messageBytes[cursor + i])
						inCodeString = inCodeString .. stringChar
					end
					keyP["in_code"] = inCodeString
					cursor = cursor + 16
					local inVersionString = ""
					for i= 0,3 do 
						local stringChar = string.char(messageBytes[cursor + i])
						inVersionString = inVersionString .. stringChar
					end
					keyP["in_version"] = inVersionString
					cursor = cursor + 4
				end
			end
			if (messageBytes[cursor + 0] == 0xAC and messageBytes[cursor + 1] == 0x00) then
				if(messageBytes[cursor + 2] == 0x11 or messageBytes[cursor + 2] == 0x10 or messageBytes[cursor + 3] == 0x00) then
					cursor = cursor + 4
				else
					local outCodeString = ""
					cursor = cursor + 12
					for i= 0,15 do 
						local stringChar = string.char(messageBytes[cursor + i])
						outCodeString = outCodeString .. stringChar
					end
					keyP["out_code"] = outCodeString
					cursor = cursor + 16
					local outVersionString = ""
					for i= 0,3 do 
						local stringChar = string.char(messageBytes[cursor + i])
						outVersionString = outVersionString .. stringChar
					end
					keyP["out_version"] = outVersionString
					cursor = cursor + 4
				end
			end
			
        end
	end
	
	if(messageBytes[0] == 0xC1) then
	

		if(messageBytes[3] == 0x44) then
			
			keyP["real_time_power_10"] = (bcd2Int(messageBytes[16]) + bcd2Int(messageBytes[17])/100 + bcd2Int(messageBytes[18])/10000)*10000
			
			--keyP["real_time_power_10"] = tonumber((messageBytes[16] + messageBytes[17]/100 + messageBytes[18]/10000),10)
			
			keyP["real_time_power"] = bit.bor(bit.bor(bit.lshift(messageBytes[16],16),bit.lshift(messageBytes[17],8)),messageBytes[18]) 
			
			--keyP["real_time_power_10"] = bit.bor(bit.bor(bit.lshift(tonumber(messageBytes[16],16),16),bit.lshift(tonumber(messageBytes[17],16),8)),tonumber(messageBytes[18],16))
			
			
		
		
		end
		
		if(bit.band(messageBytes[3],0x0F) == 0x05) then
			keyP["current_humidity"] = messageBytes[4]
		end
	
	end
	if ((dataType==0x05 and messageBytes[0] == 0xB5)) then
		keyP["propertyNumber"] = messageBytes[1]
		local cursor = 1
		for i = 1,  keyP["propertyNumber"] do
			if (messageBytes[cursor + 1] == 0x7E and messageBytes[cursor + 2] == 0x00) then
				keyP["propertyNumber"] = 0
				keyP["powerValue"] = bit.band(messageBytes[cursor + 5], 0x01)
				keyP["modeValue"] = bit.band(messageBytes[cursor + 6], 0xE0)
				
				if(dataType==0x05) then
					--CA机型 11447、11451、11453、11455、11457、11459、11525、11527、11533、11535
					if deviceSN8=="11447" or deviceSN8=="11451" or deviceSN8=="11453" or deviceSN8=="11455" or deviceSN8=="11457" or deviceSN8=="11459" or deviceSN8=="11525" or deviceSN8=="11527" or deviceSN8=="11533" or deviceSN8=="11535" then
						keyP["temperature"] = bit.rshift(bit.band(messageBytes[cursor + 5], 0x7C), 2) + 0x0C
						keyP["smallTemperature"] = bit.rshift(bit.band(messageBytes[cursor + 5], 0x02), 1)
					else
						keyP["temperature"] = bit.rshift(bit.band(messageBytes[cursor + 5], 0x3E), 1) + 0x0C
						keyP["smallTemperature"] = bit.rshift(bit.band(messageBytes[cursor + 5], 0x40), 6)
					end
				end
				
				keyP["fanspeedValue"] = bit.band(messageBytes[cursor + 7], 0x7F)

				if (bit.band(messageBytes[cursor + 8], keyB["BYTE_START_TIMER_SWITCH_ON"]) == keyB["BYTE_START_TIMER_SWITCH_ON"]) then
					keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_ON"]
				else
				   keyP["openTimerSwitch"] = keyB["BYTE_START_TIMER_SWITCH_OFF"]
				end

				if (bit.band(messageBytes[cursor + 9], keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]) == keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]) then
					keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]
				else
					keyP["closeTimerSwitch"] = keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]
				end

				keyP["closeHour"] = bit.rshift(bit.band(messageBytes[cursor + 9], 0x7F), 2)
				
				keyP["closeStepMintues"] = bit.band(messageBytes[cursor + 9], 0x03)
				
				keyP["closeMin"] = 15 - bit.band(messageBytes[cursor + 10], 0x0f)
				
				keyP["closeTime"] = keyP["closeHour"] * 60 + keyP["closeStepMintues"] * 15 + keyP["closeMin"]

				keyP["openHour"] = bit.rshift(bit.band(messageBytes[cursor + 8], 0x7F), 2)
				
				keyP["openStepMintues"] = bit.band(messageBytes[cursor + 8], 0x03)
				
				keyP["openMin"] = 15 - bit.rshift(bit.band(messageBytes[cursor + 10], 0xf0),4)
				
				keyP["openTime"] = keyP["openHour"] * 60 + keyP["openStepMintues"] * 15 + keyP["openMin"]

				keyP["strongWindValue"] = bit.band(messageBytes[cursor + 12], 0x20)
				
				keyP["power_saving"] = bit.band(messageBytes[cursor + 12], 0x08)
				
				keyP["comfortableSleepValue"] = bit.band(messageBytes[cursor + 12], 0x03)
				
				keyP["comfortableSleepSwitch"] = bit.band(messageBytes[cursor + 12], 0x40)
				
				if(dataType==0x05) then
					keyP["comfortableSleepSwitch"] = bit.band(messageBytes[cursor + 18], 0x01)
					keyP["naturalWind"] = bit.band(messageBytes[cursor + 14], 0x40)
					keyP["screenDisplayNowValue"] = bit.band(messageBytes[cursor + 15], 0x07)
					keyP["pmv"] = bit.rshift(bit.band(messageBytes[cursor + 15], 0xF0),4)* 0.5 - 3.5
					keyP["swingLRValueUnder"] = bit.band(messageBytes[cursor + 13], 0x40)
				end
				
				keyP["PTCValue"] = bit.band(messageBytes[cursor + 13], 0x08)
				
				keyP["purifierValue"] = bit.band(messageBytes[cursor + 13], 0x20)
				keyP["inner_purifier"] = bit.rshift(bit.band(messageBytes[cursor + 13], 0x20),5)
				
				keyP["ecoValue"] = bit.lshift(bit.band(messageBytes[cursor + 13], 0x10), 3)
				
				keyP["dryValue"] = bit.band(messageBytes[cursor + 13], 0x04)
				
				keyP["temperature_unit"] = bit.rshift(bit.band(messageBytes[cursor + 13], 0x80),7)
				
				
				keyP["swingLRValue"] = bit.band(messageBytes[cursor + 11], 0x03)
			
				
				keyP["wind_swing_lr_right"] = bit.band(messageBytes[cursor + 11], 0x01)
				keyP["wind_swing_lr_left"] = bit.band(messageBytes[cursor + 11], 0x02)
				
				keyP["swingUDValue"] = bit.band(messageBytes[cursor + 11], 0x0C)
				
				keyP["wind_swing_ud_right"] = bit.band(messageBytes[cursor + 11], 0x04)
				
				keyP["wind_swing_ud_left"] = bit.band(messageBytes[cursor + 11], 0x08)
				
				
				keyP["swingLRUnderSwitch"] = bit.band(messageBytes[cursor + 23], 0x80)	
				--keyP["errorCode"]=messageBytes[cursor + 20]
				
				
				
				--是否踢被子
				keyP["kickQuilt"] = bit.rshift(bit.band(messageBytes[cursor + 14], 0x04),2)
				
				--防着凉
				keyP["preventCold"] = bit.rshift(bit.band(messageBytes[cursor + 14], 0x08),3)
			
				--温度(低于17度)
				local temp = bit.rshift(bit.band(messageBytes[cursor + 16], 0x3E),1)
				if (temp > 0 and temp <= 25)  then 
					keyP["temperature"] = temp + 12 
				end
				if(messageBytes[cursor + 4] == 0xA0) then
					keyP["arom_old"] = bit.rshift(bit.band(messageBytes[cursor + 25], 0x80),7)

				end
				
				

				--舒省
				keyP["comfortPowerSave"] = bit.band(messageBytes[cursor + 18], 0x01)
				
				
				keyP["rewarming_dry"] = bit.rshift(bit.band(messageBytes[cursor + 27], 0x02),1)		   
				
				if(#binData >= 24) then
					keyP["wind_speed_right"] = bit.band(messageBytes[cursor + 28], 0x7F)
					
				end
				
				
				if(#binData >= 26) then
					keyP["indoor_co2"] = bit.bor(bit.lshift(messageBytes[cursor + 30], 8), messageBytes[cursor + 29])
					keyP["whirl_wind_right"] = bit.rshift(bit.band(messageBytes[cursor + 31], 0x08),3)
					keyP["whirl_wind_left"] = bit.rshift(bit.band(messageBytes[cursor + 31], 0x04),2)
					
					
					
				end	
				keyP["self_clean"] = bit.rshift(bit.band(messageBytes[cursor + 12],0x04),2)
				keyP["prevent_super_cool"] = bit.rshift(bit.band(messageBytes[cursor + 22],0x40),6)
				keyP["no_wind_sense_left"] = bit.band(messageBytes[cursor + 31], 0x01) + 1
				keyP["no_wind_sense_right"] = bit.rshift(bit.band(messageBytes[cursor + 31], 0x02),1) + 1
				keyP["moisturizing"] = bit.rshift(bit.band(messageBytes[cursor + 31], 0x80),7)
				keyP["linkage"] = bit.rshift(bit.band(messageBytes[cursor + 31], 0x20),5)
				keyP["linkage_sync"] = bit.rshift(bit.band(messageBytes[cursor + 31], 0x40),6)
				keyP["no_wind_sense"] = bit.rshift(bit.band(messageBytes[cursor + 18],0x08),3)
				keyP["prevent_straight_wind"] = bit.rshift(bit.band(messageBytes[cursor + 18],0x40),6)
				
				
				keyP["wind_swing_ud_angle"] = bit.band(messageBytes[cursor + 21],0x0F)
				keyP["degerming"] = bit.rshift(bit.band(messageBytes[cursor + 23], 0x02),1)
				
				
				
				if(messageBytes[cursor + 3] >= 33) then
					keyP["fresh_air_mode"] = bit.band(messageBytes[cursor + 37],0x0F)
					keyP["fresh_air_mode_two"] = bit.rshift(bit.band(messageBytes[cursor + 37],0x30),4)
					keyP["inner_purifier_mode"] = bit.rshift(bit.band(messageBytes[cursor + 37],0x40),6)
					keyP["moisturizing_fan_speed"] = messageBytes[cursor + 36]
					keyP["fresh_air_fan_speed"] = messageBytes[cursor + 38]
					keyP["inner_purifier_fan_speed"] = messageBytes[cursor + 39]
					keyP["indoor_humidity"] = messageBytes[cursor + 40]
					keyP["five_dimension_mode"] = bit.band(messageBytes[cursor + 41],0x03)
					keyP["total_status_switch"] = bit.rshift(bit.band(messageBytes[cursor + 41],0x04),2)
					keyP["wind_no_linkage"] = bit.rshift(bit.band(messageBytes[cursor + 41],0x10),4)
				end
				keyP["ieco_switch"] = bit.rshift(bit.band(messageBytes[cursor + 45], 0x10), 4)
				keyP["linkage_fan_speed"] = bit.band(messageBytes[cursor + 42],0x7F)
				keyP["indoorTemperatureValue"] = messageBytes[cursor + 44]
				keyP["smallIndoorTemperatureValue"] = bit.band(messageBytes[cursor + 45],0x0F)
				

				keyP["fresh_air"] = bit.rshift(bit.band(messageBytes[cursor + 22],0x80),7)
				keyP["ptc_default_rule"] = bit.rshift(bit.band(messageBytes[cursor + 27], 0x20),5)
				keyP["light_sensitive"] = bit.rshift(bit.band(messageBytes[cursor + 27], 0xC0),6)

				--新风滤网
				if(#binData >= 29) then
					keyP["fresh_filter_time_total"] = messageBytes[cursor + 29] * 256 + messageBytes[cursor + 28] 
					keyP["fresh_filter_time_use"] = messageBytes[cursor + 31] * 256 + messageBytes[cursor + 30] 
					keyP["fresh_filter_timeout"] = bit.rshift(bit.band(messageBytes[cursor + 17], 0x40),6)
					
				end
				keyP["fresh_filter_time_use"] = messageBytes[cursor + 20] * 256 + messageBytes[cursor + 19]
				
				
				cursor = cursor + 42
			end
		end
	end
    if ((dataType==0x03 and messageBytes[0] == 0xB5)) then
        --新协议，变长属性协议
		if(#binData < 4) then
			return nil
		end
		keyP["propertyNumber"] = messageBytes[1]
		local cursor = 2
        for i = 1,  keyP["propertyNumber"] do
			
			if (messageBytes[cursor + 0] == 0x14 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_mode"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x1a and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_strong_wind"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x10 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_wind_speed"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x1f and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_humidity"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x25 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_temperature_0"] = messageBytes[cursor + 3]	
				keyP["b5_temperature_1"] = messageBytes[cursor + 4]	
				keyP["b5_temperature_2"] = messageBytes[cursor + 5]	
				keyP["b5_temperature_3"] = messageBytes[cursor + 6]	
				keyP["b5_temperature_4"] = messageBytes[cursor + 7]	
				keyP["b5_temperature_5"] = messageBytes[cursor + 8]	
				keyP["b5_temperature_6"] = messageBytes[cursor + 9]	
				cursor = cursor + 10
			end
			if (messageBytes[cursor + 0] == 0x12 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_eco"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x17 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_filter_remind"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x21 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_filter_check"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x22 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_fahrenheit"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x13 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_8_heat"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x16 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_electricity"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x19 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_ptc"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x32 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_wind_straight"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x33 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_wind_avoid"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x15 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_wind_swing"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x18 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_no_wind_sense"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x24 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_screen_display"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x1e and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_anion"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x39 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_self_clean"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			
			if (messageBytes[cursor + 0] == 0x30 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_nobody_energy_save"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x42 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_prevent_straight_wind"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x67 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_jet_cool"] = messageBytes[cursor + 3]				
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x34 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_body_check"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x48 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_rate_select"] = messageBytes[cursor + 3]	
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x43 and messageBytes[cursor + 1] == 0x00) then
				if(messageBytes[cursor + 3]	== 1)then
					keyP["b5_prevent_straight_wind"] = messageBytes[cursor + 3]	
					keyP["b5_fa_no_wind_sense"] = messageBytes[cursor + 3]
					keyP["prevent_straight_wind_flag"] = 43
				end
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x09 and messageBytes[cursor + 1] == 0x00) then	
				keyP["b5_wind_swing_ud_angle"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x4B and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_fresh_air"] =  messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x0A and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_wind_swing_lr_angle"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x30 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_main_horizontal_guide_strip_1"] =  messageBytes[cursor + 3]
				keyP["b5_main_horizontal_guide_strip_2"] =  messageBytes[cursor + 4]
				keyP["b5_main_horizontal_guide_strip_3"] =  messageBytes[cursor + 5]
				keyP["b5_main_horizontal_guide_strip_4"] =  messageBytes[cursor + 6]
				cursor = cursor + 7
			end
			if (messageBytes[cursor + 0] == 0x31 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_sup_horizontal_guide_strip_1"] =  messageBytes[cursor + 3]
				keyP["b5_sup_horizontal_guide_strip_2"] =  messageBytes[cursor + 4]
				keyP["b5_sup_horizontal_guide_strip_3"] =  messageBytes[cursor + 5]
				keyP["b5_sup_horizontal_guide_strip_4"] =  messageBytes[cursor + 6]
				cursor = cursor + 7
			end
			if (messageBytes[cursor + 0] == 0x32 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_twins_machine"] =  messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x33 and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_guide_strip_type"] =  messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x2C and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_sound"] =  messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x1E and messageBytes[cursor + 1] == 0x02) then
				keyP["b5_anion"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			
			if (messageBytes[cursor + 0] == 0x59 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_wind_around"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x51 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_parent_control"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x58 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_prevent_straight_wind_select"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			
			if (messageBytes[cursor + 0] == 0x91 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_has_icheck"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x93 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_emergent_heat_wind"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x94 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_heat_ptc_wind"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x98 and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_cvp"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0xAA and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_new_wind_sense"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x6D and messageBytes[cursor + 1] == 0x00) then
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0xAD and messageBytes[cursor + 1] == 0x00) then
				keyP["b5_comfort"] = messageBytes[cursor + 3]
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0x4D and messageBytes[cursor + 1] == 0x00) then
				cursor = cursor + 5
			end
			if (messageBytes[cursor + 0] == 0x1A and messageBytes[cursor + 1] == 0x00) then
				cursor = cursor + 4
			end
			if (messageBytes[cursor + 0] == 0xE3 and messageBytes[cursor + 1] == 0x00) then
				if(messageBytes[cursor + 2] == 1)then
					keyP["b5_ieco_switch"] = messageBytes[cursor + 3]
					cursor = cursor + 4
				else
					keyP["b5_ieco_switch"] = messageBytes[cursor + 3]
					keyP["b5_end_ieco"] = messageBytes[cursor + 4]
					cursor = cursor + 5
				end
			end
		end
    end
end

local function  getTotalMsg(bodyData,cType)
    local bodyLength = #bodyData 
	
    local msgLength = bodyLength + keyB["BYTE_PROTOCOL_LENGTH"] + 1
	
    local msgBytes = {}
	
    for i = 0, msgLength do
        msgBytes[i] = 0
    end
     
    --构造消息部分
    msgBytes[0] = keyB["BYTE_PROTOCOL_HEAD"]
	
    msgBytes[1] = bodyLength + keyB["BYTE_PROTOCOL_LENGTH"] + 1
	
    msgBytes[2] = keyB["BYTE_DEVICE_TYPE"]
	
    if (keyP["propertyNumber"] > 0) then
		msgBytes[8] = 0x02
	end
    msgBytes[9] = cType
	
    -- body
    for i = 0, bodyLength do
        msgBytes[i + keyB["BYTE_PROTOCOL_LENGTH"]] = bodyData[i]
    end
        
    msgBytes[msgLength] = makeSum(msgBytes, 1, msgLength - 1)

    local msgFinal = {}
    
    for i = 1, msgLength + 1  do
        msgFinal[i] = msgBytes[i - 1]
    end
    return msgFinal
end

--json转二进制，可传入原状态
function jsonToData(jsonCmd)
    if (#jsonCmd == 0) then
        return nil
    end
    
    local infoM = {}
    local bodyBytes = {}
	local prevent_temp = 0
    
    local json = decode(jsonCmd)
    deviceSubType = json["deviceinfo"]["deviceSubType"]
    local deviceSN=json["deviceinfo"]["deviceSN"]
    if deviceSN~=nil then
        deviceSN8=string.sub(deviceSN,13,17)
    end

    local query = json["query"]
    local control = json["control"]
    local status = json["status"]
    
    --当前是查询指令，构造固定的二进制即可
    if (query) then
        --构造消息 body 部分
        local queryType = nil
        if (type(query) == "table") then
            queryType = query["query_type"]
        end
        if (queryType == nil) then
            for i = 0, 21 do
                bodyBytes[i] = 0
            end

            bodyBytes[0] = 0x41

            bodyBytes[1] = 0x81

            bodyBytes[3] = 0xFF

            math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
            math.random()
            bodyBytes[20] = math.random(1, 254)

            bodyBytes[21] = crc8_854(bodyBytes, 0, 20)

            infoM = getTotalMsg(bodyBytes,keyB["BYTE_QUERYL_REQUEST"])
        elseif (queryType == "power"
                or queryType == "purifier"
                or queryType == "mode"
                or queryType == "temperature"
                or queryType == "small_temperature"
                or queryType == "buzzer"
                or queryType == "wind_swing_lr"
                or queryType == "wind_swing_lr_under"
                or queryType == "wind_swing_ud"
                or queryType == "wind_speed"
                or queryType == "power_on_timer"
                or queryType == "power_off_timer"
                or queryType == "power_on_time_value"
                or queryType == "power_off_time_value"
                or queryType == "indoor_temperature"
                or queryType == "outdoor_temperature"
                or queryType == "eco"
                or queryType == "kick_quilt"
                or queryType == "prevent_cold"
                or queryType == "dry"
                or queryType == "ptc"
                or queryType == "screen_display"
                or queryType == "screen_display_now"
                or queryType == "strong_wind"
                or queryType == "tubro"
                or queryType == "current_work_time"
                or queryType == "comfort_power_save"
				or queryType == "comfort_sleep"
				or queryType == "natural_wind"
				or queryType == "power_saving"
				or queryType == "fresh_filter_time_total"
				or queryType == "fresh_filter_time_use"
				or queryType == "fresh_filter_timeout"
				or queryType == "fresh_filter_timeout_ae2"
				or queryType == "independent_ptc"
        ) then
            for i = 0, 21 do
                bodyBytes[i] = 0
            end

            bodyBytes[0] = 0x41

            bodyBytes[1] = 0x81

            bodyBytes[3] = 0xFF

            math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
            math.random()
            bodyBytes[20] = math.random(1, 254)

            bodyBytes[21] = crc8_854(bodyBytes, 0, 20)

            infoM = getTotalMsg(bodyBytes,keyB["BYTE_QUERYL_REQUEST"])
		elseif (queryType == "a0_query") then
			--for i = 0, 21 do
                --bodyBytes[i] = 0
            --end
			--infoM = getTotalMsg(bodyBytes,0xa0)
			infoM[1] = 0xaa
            infoM[2] = 0x0a
            infoM[3] = 0xac
            infoM[4] = 0x00
            infoM[5] = 0x00
            infoM[6] = 0x00
            infoM[7] = 0x00
            infoM[8] = 0x00
            infoM[9] = 0x03
            infoM[10] = 0xa0
            infoM[11] = 0xa7
		elseif (queryType == "a0_query_long") then
			for i = 1, 31 do
                infoM[i] = 0
            end
			infoM[1] = 0xaa
            infoM[2] = 0x1e
            infoM[3] = 0xac
            infoM[4] = 0x00
            infoM[5] = 0x00
            infoM[6] = 0x00
            infoM[7] = 0x00
            infoM[8] = 0x00
            infoM[9] = 0x03
            infoM[10] = 0xa0
            --infoM[11] = 0xa7
			infoM[31] = makeSum(infoM, 2, 30)
		
        elseif (queryType == "all_first_frame") then
            infoM[1] = 0xaa
            infoM[2] = 0x0e
            infoM[3] = 0xac
            infoM[4] = 0x00
            infoM[5] = 0x00
            infoM[6] = 0x00
            infoM[7] = 0x00
            infoM[8] = 0x00
            infoM[9] = 0x03
            infoM[10] = 0x03
            infoM[11] = 0xb5
            infoM[12] = 0x01
            infoM[13] = 0x00
            --infoM[14] = 0x4d
			infoM[14] = crc8_854(infoM, 11, 13)
            infoM[15] = 0x3d
			--infoM[15] = makeSum(infoM, 2, 14)
        elseif (queryType == "all_second_frame") then
            infoM[1] = 0xaa
            infoM[2] = 0x0f
            infoM[3] = 0xac
            infoM[4] = 0x00
            infoM[5] = 0x00
            infoM[6] = 0x00
            infoM[7] = 0x00
            infoM[8] = 0x00
            infoM[9] = 0x03
            infoM[10] = 0x03
            infoM[11] = 0xb5
            infoM[12] = 0x01
            infoM[13] = 0x01
            infoM[14] = 0x01
            infoM[15] = 0x21
            infoM[16] = 0x66
		elseif (queryType == "group_data_four") then
            for i = 0, 21 do
                bodyBytes[i] = 0
            end

            bodyBytes[0] = 0x41

            bodyBytes[1] = 0x21

            bodyBytes[2] = 0x01
			
			bodyBytes[3] = 0x44
			
            math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
            math.random()
            bodyBytes[20] = math.random(1, 254)

            bodyBytes[21] = crc8_854(bodyBytes, 0, 20)

            infoM = getTotalMsg(bodyBytes,keyB["BYTE_QUERYL_REQUEST"])
		elseif (queryType == "group_data_five") then
            for i = 0, 21 do
                bodyBytes[i] = 0
            end

            bodyBytes[0] = 0x41

            bodyBytes[1] = 0x21

            bodyBytes[2] = 0x01
			
			bodyBytes[3] = 0x45
			
            math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
            math.random()
            bodyBytes[20] = math.random(1, 254)

            bodyBytes[21] = crc8_854(bodyBytes, 0, 20)

            infoM = getTotalMsg(bodyBytes,keyB["BYTE_QUERYL_REQUEST"])
        else
            bodyBytes[0] = 0xB1
            local propertyNum = 0

			local queryList = {}
            if (string.match(queryType,",")==",") then
				queryList  = splitStrByChar(queryType,",")
			else
				table.insert(queryList, queryType)
			end

			for v in values(queryList) do
				queryType = v
				if (queryType == "no_wind_sense") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x18
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				
				if (queryType == "fa_no_wind_sense") then
				    bodyBytes[1 + propertyNum * 2 + 1] = 0x43
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "cool_hot_sense") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x21
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "nobody_energy_save") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x30
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "self_clean") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x39
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "child_prevent_cold_wind") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x3A
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "error_code_query") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x3F
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "mode_query") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x41
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "prevent_straight_wind") then
					if(prevent_temp == 0) then
						prevent_temp = 1
					end
				end
				if (queryType == "prevent_straight_wind_flag") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x43
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
					prevent_temp = 2
				end
				if (queryType == "prevent_super_cool") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x49
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "high_temperature_monitor") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x47
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "rate_select") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x48
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "intelligent_wind") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x34
					if (deviceSN8 == "50939" or deviceSN8 == "51001" or deviceSN8 == "Z1304" or deviceSN8 == "Z1259" or deviceSN8 == "Z2272") then
						bodyBytes[1 + propertyNum * 2 + 1] = 0x33
					end
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "wind_straight" or queryType == "yb_wind_avoid") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x32
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "wind_avoid") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x33
					if (deviceSN8 == "50939" or deviceSN8 == "51001" or deviceSN8 == "Z1304" or deviceSN8 == "Z1259" or deviceSN8 == "Z2272") then
						bodyBytes[1 + propertyNum * 2 + 1] = 0x32
					end
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "auto_prevent_straight_wind") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x26
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "security") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x29
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "even_wind") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x4E
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "single_tuyere") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x4F
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "extreme_wind") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x4C
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "voice_control") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x20
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "pre_cool_hot") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x01
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "water_washing") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x4A
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "fresh_air") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x4B
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "parent_control") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x51
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "filter_value") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x09
					bodyBytes[1 + propertyNum * 2 + 2] = 0x04
					propertyNum = propertyNum + 1
				end
				if (queryType == "wind_swing_ud_angle") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x09
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "wind_swing_lr_angle") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x0A
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				
				if (queryType == "pm25_value") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x0B
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "water_pump") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x50
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "intelligent_control") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x31
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "volume_control") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x24
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "voice_control_new") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x20
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "face_register") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x44
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "cool_temp_down" or queryType == "cool_temp_up" or queryType == "auto_temp_down" or queryType == "auto_temp_up" or queryType == "heat_temp_down" or queryType == "heat_temp_up") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x25
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "remote_control_lock") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x27
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "operating_time") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x28
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "indoor_humidity") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x15
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "jet_cool") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x67
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "body_check") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x34
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "main_horizontal_guide_strip_1" or queryType == "main_horizontal_guide_strip_2" or queryType == "main_horizontal_guide_strip_3" or queryType == "main_horizontal_guide_strip_4") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x30
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "sup_horizontal_guide_strip_1" or queryType == "sup_horizontal_guide_strip_2" or queryType == "sup_horizontal_guide_strip_3" or queryType == "sup_horizontal_guide_strip_4") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x31
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "twins_machine") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x32
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "guide_strip_type") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x33
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "sound") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x2C
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "anion") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x1E
					bodyBytes[1 + propertyNum * 2 + 2] = 0x02
					propertyNum = propertyNum + 1
				end
				if (queryType == "ieco_switch") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0xE3
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "wind_around") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x59
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "prevent_straight_wind_select" or queryType == "prevent_straight_wind_lr") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x58
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "mito_cool") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x8D
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "mito_heat") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x8E
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "dr_time" or queryType == "dr_time_hour") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x8F
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "has_cool_heat_amount") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x90
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "has_icheck") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x91
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "cvp") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0x98
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "new_wind_sense") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0xAA
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "in_code_query") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0xAB
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "out_code_query") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0xAC
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
				if (queryType == "comfort") then
					bodyBytes[1 + propertyNum * 2 + 1] = 0xAD
					bodyBytes[1 + propertyNum * 2 + 2] = 0x00
					propertyNum = propertyNum + 1
				end
			
			end
			if(prevent_temp == 1) then
				bodyBytes[1 + propertyNum * 2 + 1] = 0x42
				bodyBytes[1 + propertyNum * 2 + 2] = 0x00
				propertyNum = propertyNum + 1
			end
            bodyBytes[1] = propertyNum

            math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
            math.random()
            bodyBytes[1 + propertyNum * 2 + 1] = math.random(1, 254)

            bodyBytes[1 + propertyNum * 2 + 2] = crc8_854(bodyBytes, 0, 1 + propertyNum * 2 + 1)

            infoM = getTotalMsg(bodyBytes,keyB["BYTE_QUERYL_REQUEST"])
        end
    elseif (control) then
        --先将原始状态转换为属性
        if (status) then
            jsonToModel(status,"status")
        end
        keyP["ieco_status"] = nil
        --将用户控制 json 转换为属性
        if (control) then
			if(control[keyT["KEY_SCREEN_DISPLAY"]] ~= nil) then
				for i = 0, 22 do
					bodyBytes[i] = 0
				end
				bodyBytes[0] = 0x41
				bodyBytes[1] = 0x61
				if(control[keyT["KEY_BUZZER"]] ~= nil and control[keyT["KEY_BUZZER"]] == "off") then
					bodyBytes[1] = 0x61
				end
				bodyBytes[3] = 0xFF
				bodyBytes[4] = 0x02
				bodyBytes[5] = 0x00
				bodyBytes[6] = 0x02
				bodyBytes[7] = 0x00
				math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
				math.random()
				bodyBytes[21] = math.random(1, 254)
				bodyBytes[22] = crc8_854(bodyBytes, 0, 21)
				infoM = getTotalMsg(bodyBytes,keyB["BYTE_QUERYL_REQUEST"])
				--table 转换成 string 之后返回
				local ret = table2string(infoM)
				ret = string2hexstring(ret)
				return ret
			end
            jsonToModel(control,"control")
			if(control["ptc"] ~= nil and (control["ptc"] == "on" or control["ptc"] == "off")) then 
				keyP["PTCForceValue"] = 1
			else 
				keyP["PTCForceValue"] = 0
			end
        end


        --构造消息 body 部分
        if (keyP["propertyNumber"] == 0) then
		--常规协议 
		for i = 0, 25 do
            bodyBytes[i] = 0
        end
		
        bodyBytes[0] = keyB["BYTE_CONTROL_CMD"]
		
        bodyBytes[1] = bit.bor(bit.bor(keyP["powerValue"], keyB["BYTE_CLIENT_MODE_MOBILE"]), bit.bor(keyB["BYTE_TIMER_METHOD_REL"], keyP["buzzerValue"]))
		
        --bodyBytes[2] = bit.bor(bit.band(keyP["modeValue"], 0xE0), bit.band(0x0F, (keyP["temperature"] - 0x10)))
		if(keyP["temperature"] >= 16)then
			bodyBytes[2] = bit.bor(bit.bor(bit.band(keyP["modeValue"], 0xE0), bit.band(0x0F, (keyP["temperature"] - 0x10))),bit.lshift(bit.band(keyP["smallTemperature"], 0x01),4))
		else
			bodyBytes[2] = bit.bor(bit.bor(bit.band(keyP["modeValue"], 0xE0), 0),bit.lshift(bit.band(keyP["smallTemperature"], 0x01),4))
		end
        bodyBytes[3] = bit.bor(keyP["fanspeedValue"], keyB["BYTE_TIMER_SWITCH_ON"])

        if (keyP["closeTime"] == nil) then
            keyP["closeTime"] = 0
        end
		
        keyP["closeHour"] = math.floor(keyP["closeTime"] / 60)
		
        keyP["closeStepMintues"] = math.floor((keyP["closeTime"] % 60) / 15)
		
        keyP["closeMin"] = math.floor(((keyP["closeTime"] % 60) % 15))

        if (keyP["openTime"] == nil) then
            keyP["openTime"] = 0
        end
		
        keyP["openHour"] = math.floor(keyP["openTime"] / 60)
		
        keyP["openStepMintues"] = math.floor((keyP["openTime"] % 60) / 15)
		
        keyP["openMin"] = math.floor(((keyP["openTime"] % 60) % 15))

        if (keyP["openTimerSwitch"] == keyB["BYTE_START_TIMER_SWITCH_ON"]) then
            bodyBytes[4] = bit.bor(bit.bor(keyP["openTimerSwitch"], bit.lshift(keyP["openHour"], 2)), keyP["openStepMintues"])
        elseif (keyP["openTimerSwitch"] == keyB["BYTE_START_TIMER_SWITCH_OFF"] ) then
            bodyBytes[4] = 0x7F
        end

        if (keyP["closeTimerSwitch"] == keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]) then
            bodyBytes[5] = bit.bor(bit.bor(keyP["closeTimerSwitch"], bit.lshift(keyP["closeHour"], 2)), keyP["closeStepMintues"])
        elseif (keyP["closeTimerSwitch"] == keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]) then
            bodyBytes[5] = 0x7F
        end

        bodyBytes[6] = bit.bor(bit.lshift((15 - keyP["openMin"]), 4), (15 - keyP["closeMin"]))

        bodyBytes[7] = bit.bor(bit.bor(keyP["swingLRValue"], keyP["swingUDValue"]), 0x30)
		
        bodyBytes[8] = bit.bor(bit.bor(keyP["strongWindValue"], keyP["comfortableSleepValue"]), keyP["power_saving"])
		
        bodyBytes[9] = bit.bor(bit.bor(bit.bor(keyP["purifierValue"], keyP["ecoValue"]), bit.bor(keyP["dryValue"], keyP["PTCValue"])),keyP["comfortableSleepSwitch"])
		if ((keyP["PTCForceValue"] ~= nil) and (keyP["PTCForceValue"] == 1)) then
			bodyBytes[9] = bit.bor(bodyBytes[9], 0x10)
		end
        bodyBytes[10] = bit.lshift(bit.band(keyP["preventCold"], 0x01), 3)
		bodyBytes[10] = bit.bor(keyP["sleep_status"], bodyBytes[10])
		if(keyP["temperature_unit"] ~= nil) then
			bodyBytes[10] = bit.bor(bit.lshift(bit.band(keyP["temperature_unit"],0x01), 2), bodyBytes[10])
		end	
		
		if(keyP["common_filter_reset"] ~= nil) then 
			bodyBytes[10] = bit.bor(keyP["common_filter_reset"], bodyBytes[10])
		end
		if(keyP["tubroValue"] ~= nil) then 
			bodyBytes[10] = bit.bor(keyP["tubroValue"], bodyBytes[10])
		end
		
		if(keyP["comfortableSleepValue"] == keyB["BYTE_SLEEP_ON"] and comfortByte == nil) then 
		    if(keyP["modeValue"] == keyB["BYTE_MODE_HEAT"]) then
		        firstHourTemp = checkBoundary(keyP["temperature"] - 1, 17, 30)
			    otherHourTemp = checkBoundary(keyP["temperature"] - 2, 17, 30)
		    else
		        firstHourTemp = checkBoundary(keyP["temperature"] + 1, 17, 30)
			    otherHourTemp = checkBoundary(keyP["temperature"] + 2, 17, 30)
		    end
		
		    bodyBytes[11] = bit.bor(firstHourTemp -17,bit.lshift((otherHourTemp -17), 4))
		    bodyBytes[12] = bit.bor(otherHourTemp -17,bit.lshift((otherHourTemp -17), 4))
		    bodyBytes[13] = bit.bor(otherHourTemp -17,bit.lshift((otherHourTemp -17), 4))
		    bodyBytes[14] = bit.bor(otherHourTemp -17,bit.lshift((otherHourTemp -17), 4))
		    bodyBytes[15] = bit.bor(otherHourTemp -17,bit.lshift((otherHourTemp -17), 4))
		
		    if(keyP["smallTemperature"] ~= 0) then 
		        bodyBytes[16] = 0xFF
				bodyBytes[17] = bit.bor(keyP["comfortableSleepTime"], 0x30)
			else
			    bodyBytes[17] = keyP["comfortableSleepTime"]
			end
			
		elseif(keyP["comfortableSleepValue"] == keyB["BYTE_SLEEP_ON"] and comfortByte ~= nil) then
		    bodyBytes[11] = bit.bor(checkBoundary(comfortByte[1], 17, 30) -17,bit.lshift((checkBoundary(comfortByte[2], 17, 30) -17), 4))
		    bodyBytes[12] = bit.bor(checkBoundary(comfortByte[3], 17, 30) -17,bit.lshift((checkBoundary(comfortByte[4], 17, 30) -17), 4))
		    bodyBytes[13] = bit.bor(checkBoundary(comfortByte[5], 17, 30) -17,bit.lshift((comfortByte[6] -17), 4))
		    bodyBytes[14] = bit.bor(checkBoundary(comfortByte[7], 17, 30) -17,bit.lshift((comfortByte[8] -17), 4))
		    bodyBytes[15] = bit.bor(checkBoundary(comfortByte[9], 17, 30) -17,bit.lshift((comfortByte[10] -17), 4))
		   
		    if(keyP["smallTemperature"] ~= 0) then 
		        bodyBytes[16] = 0xFF
				bodyBytes[17] = bit.bor(keyP["comfortableSleepTime"], 0x30)
		    else
			    bodyBytes[17] = keyP["comfortableSleepTime"]
		    end
	
		end
		if(keyP["pmv"] ~= nil) then
			local pmvValue = (keyP["pmv"] + 3.5) * 2 
			bodyBytes[17] = bit.bor( bit.lshift(bit.band(pmvValue,0x08), 4), bodyBytes[17])
			bodyBytes[18] = bit.bor( bit.lshift(bit.band(pmvValue,0x07), 5), bodyBytes[18])
		end
		if(keyP["naturalWind"] ~= nil) then 
			bodyBytes[17] = bit.bor(keyP["naturalWind"], bodyBytes[17])
		end

		if (keyP["temperature"] < 17 or keyP["temperature"] > 30) then
			if((keyP["temperature"] >= 13 and keyP["temperature"] < 17) or (keyP["temperature"] <= 38 and keyP["temperature"] > 30))then
				bodyBytes[18] = bit.bor( bit.band(0x1F, (keyP["temperature"] - 12)), bodyBytes[18])
			else
				bodyBytes[18] = bit.bor( bit.band(0x1F, (keyP["temperature"] + 19)), bodyBytes[18])
			end
		end
		
		if((keyP["modeValue"] == keyB["BYTE_MODE_SMART_DRY"] or keyP["modeValue"] == keyB["BYTE_MODE_DRY"]) and keyP["smartDryValue"] ~= nil) then
			bodyBytes[19] = bit.bor( bit.band(0x7F,keyP["smartDryValue"]), bodyBytes[19])
		end
	
		if(keyP["swingLRUnderSwitch"] ~= nil) then
			bodyBytes[19] = bit.bor(keyP["swingLRUnderSwitch"], bodyBytes[19])
		end
		if(keyP["swingLRValueUnder"] ~= nil) then
			bodyBytes[20] = bit.bor(keyP["swingLRValueUnder"], bodyBytes[20])
        end
		if(keyP["degree8_heat"] ~= nil) then
			bodyBytes[21] = bit.lshift(bit.band(keyP["degree8_heat"], 0x01), 7)
		end

		if(keyP["comfortPowerSave"] ~= nil) then
			if (keyP["comfortPowerSave"] == keyB["BYTE_COMFORT_POWER_SAVE_ON"]) then
				bodyBytes[22] = 0x01
			elseif (keyP["comfortPowerSave"] == keyB["BYTE_COMFORT_POWER_SAVE_OFF"] ) then
				bodyBytes[22] = 0x00
			end
		end
        if(keyP["fresh_filter_reset"] ~= nil) then 
			bodyBytes[22] = bit.bor(keyP["fresh_filter_reset"], bodyBytes[22])
		end
		if(keyP["independent_ptc"] ~= nil) then 
			bodyBytes[22] = bit.bor(keyP["independent_ptc"], bodyBytes[22])
		end
		math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
        math.random()
        bodyBytes[24] = math.random(1, 254)
		
        bodyBytes[25] = crc8_854(bodyBytes, 0, 24)
		
		
			
		else
			--新协议，属性变长协议
			bodyBytes[0] = keyB["BYTE_CONTROL_PROPERTY_CMD"]
			bodyBytes[1] = keyP["propertyNumber"]
			local cursor = 2
			if(keyP["prevent_super_cool"] ~= nil) then
				bodyBytes[cursor + 0] = 0x49
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x05
                bodyBytes[cursor + 3] = keyP["prevent_super_cool"]
                bodyBytes[cursor + 4] = 0xFF
                bodyBytes[cursor + 5] = 0xFF
                bodyBytes[cursor + 6] = 0xFF
                bodyBytes[cursor + 7] = 0xFF
                cursor = cursor + 8
			end
			if(keyP["prevent_straight_wind"] ~= nil) then
					bodyBytes[cursor + 0] = 0x42
					if(keyP["prevent_straight_wind_flag"] ~=nil and keyP["prevent_straight_wind_flag"] == 43) then
						bodyBytes[cursor + 0] = 0x43
					end
					bodyBytes[cursor + 1] = 0x00 	
					bodyBytes[cursor + 2] = 0x01 	
					bodyBytes[cursor + 3] = keyP["prevent_straight_wind"]
					cursor = cursor + 4
			end
			if(keyP["auto_prevent_straight_wind"] ~= nil) then
				bodyBytes[cursor + 0] = 0x26 	
				bodyBytes[cursor + 1] = 0x02 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["auto_prevent_straight_wind"]
				cursor = cursor + 4
			end
			if(keyP["self_clean"] ~= nil) then
				bodyBytes[cursor + 0] = 0x39 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["self_clean"]
				cursor = cursor + 4
			end
			if(keyP["gentle_wind_sense"] ~= nil) then
				bodyBytes[cursor + 0] = 0x43 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["gentle_wind_sense"]
				cursor = cursor + 4
			end
			if(keyP["wind_straight"] ~= nil) then
				bodyBytes[cursor + 0] = 0x32 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["wind_straight"]
				cursor = cursor + 4
			end
			if(keyP["yb_wind_avoid"] ~= nil) then
				bodyBytes[cursor + 0] = 0x32 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["yb_wind_avoid"]
				cursor = cursor + 4
			end
			if(keyP["wind_avoid"] ~= nil) then
			    --YB100特殊处理
				if (deviceSN8 == "50939" or deviceSN8 == "51001" or deviceSN8 == "Z1304" or deviceSN8 == "Z1259" or deviceSN8 == "Z2272") then
					bodyBytes[cursor + 0] = 0x32		
				    bodyBytes[cursor + 1] = 0x00 	
				    bodyBytes[cursor + 2] = 0x01
                    if(keyP["wind_avoid"] == 0x01) then
                        bodyBytes[cursor + 3] = 0x02
                    else						
				        bodyBytes[cursor + 3] = 0x00
					end
				    cursor = cursor + 4
                else			
				    bodyBytes[cursor + 0] = 0x33		
				    bodyBytes[cursor + 1] = 0x00 	
				    bodyBytes[cursor + 2] = 0x01 	
				    bodyBytes[cursor + 3] = keyP["wind_avoid"]
				    cursor = cursor + 4
				end
			end
			if(keyP["intelligent_wind"] ~= nil) then
				bodyBytes[cursor + 0] = 0x34 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["intelligent_wind"]
				cursor = cursor + 4
			end
			if(keyP["no_wind_sense"] ~= nil) then
				--FA100特殊处理
				if (deviceSN8 == "12035"or deviceSN8 == "12037" or deviceSN8 == "Z1312" or deviceSN8 == "Z1262" or deviceSN8 == "12179" or deviceSN8 == "Z1261") then
					bodyBytes[cursor + 0] = 0x43 	
					bodyBytes[cursor + 1] = 0x00 	
					bodyBytes[cursor + 2] = 0x01
					bodyBytes[cursor + 3] = 0x00
					if (keyP["no_wind_sense"] == 1) then
						bodyBytes[cursor + 3] = 0x04
					elseif (keyP["no_wind_sense"] == 0) then 
						bodyBytes[cursor + 3] = 0x01
					end
					cursor = cursor + 4
				elseif (deviceSN8 == "51023") then
					bodyBytes[cursor + 0] = 0x18 	
					bodyBytes[cursor + 1] = 0x00 	
					bodyBytes[cursor + 2] = 0x02 	
					bodyBytes[cursor + 3] = keyP["no_wind_sense"]
					bodyBytes[cursor + 4] = keyP["no_wind_sense_level"]
					cursor = cursor + 5
                else
	                bodyBytes[cursor + 0] = 0x18 	
					bodyBytes[cursor + 1] = 0x00 	
					bodyBytes[cursor + 2] = 0x01 	
					bodyBytes[cursor + 3] = keyP["no_wind_sense"]
					cursor = cursor + 4			
				end
			end
			if(keyP["fa_no_wind_sense"] ~= nil) then
				bodyBytes[cursor + 0] = 0x43 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["fa_no_wind_sense"]
				cursor = cursor + 4	
			end
			if(keyP["child_prevent_cold_wind"] ~= nil) then
				bodyBytes[cursor + 0] = 0x3A 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["child_prevent_cold_wind"]
				cursor = cursor + 4
			end
			if(keyP["little_angel"] ~= nil) then
				bodyBytes[cursor + 0] = 0x1B 	
				bodyBytes[cursor + 1] = 0x02 	
				bodyBytes[cursor + 2] = 0x01 	
				bodyBytes[cursor + 3] = keyP["little_angel"]
				cursor = cursor + 4
			end
			if(keyP["cool_hot_sense"] ~= nil) then
				bodyBytes[cursor + 0] = 0x21 	
				bodyBytes[cursor + 1] = 0x00 	
				bodyBytes[cursor + 2] = 0x08 	
				bodyBytes[cursor + 3] = keyP["cool_hot_sense"]
				bodyBytes[cursor + 4] = 0x00
				bodyBytes[cursor + 5] = 0x00
				bodyBytes[cursor + 6] = 0x00
				bodyBytes[cursor + 7] = 0x00
				bodyBytes[cursor + 8] = 0x00
				bodyBytes[cursor + 9] = 0x00
				bodyBytes[cursor + 10] = 0x00
				cursor = cursor + 11
			end
			if(keyP["even_wind"] ~= nil) then
				bodyBytes[cursor + 0] = 0x4E
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["even_wind"]
				cursor = cursor + 4
			end
			if(keyP["single_tuyere"] ~= nil) then
				bodyBytes[cursor + 0] = 0x4F
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["single_tuyere"]
				cursor = cursor + 4
			end
			if(keyP["extreme_wind"] ~= nil) then
				bodyBytes[cursor + 0] = 0x4C
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x02
				bodyBytes[cursor + 3] = keyP["extreme_wind"]
				bodyBytes[cursor + 4] = 0x01
				cursor = cursor + 5
			end
			if(keyP["security"] ~= nil) then
				bodyBytes[cursor + 0] = 0x29
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["security"]
				cursor = cursor + 4
			end
			if(keyP["voice_control"] ~= nil) then
				bodyBytes[cursor + 0] = 0x20
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x09
				bodyBytes[cursor + 3] = keyP["voice_control"]
				bodyBytes[cursor + 4] = 0xFF
				bodyBytes[cursor + 5] = 0xFF
				bodyBytes[cursor + 6] = 0xFF
				bodyBytes[cursor + 7] = 0xFF
				bodyBytes[cursor + 8] = 0xFF
				bodyBytes[cursor + 9] = 0xFF
				bodyBytes[cursor + 10] = 0xFF
				bodyBytes[cursor + 11] = 0xFF
				cursor = cursor + 12
			end
			if(keyP["pre_cool_hot"] ~= nil) then
				bodyBytes[cursor + 0] = 0x01
				bodyBytes[cursor + 1] = 0x02
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["pre_cool_hot"]
				cursor = cursor + 4
			end
			if(keyP["water_washing"] ~= nil) then
				bodyBytes[cursor + 0] = 0x4A
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x04
				bodyBytes[cursor + 3] = keyP["water_washing_manual"]
				bodyBytes[cursor + 4] = keyP["water_washing"]
				bodyBytes[cursor + 5] = keyP["water_washing_time"]
				bodyBytes[cursor + 6] = 0xFF
				cursor = cursor + 7
			end
			if(keyP["fresh_air"] ~= nil) then
				bodyBytes[cursor + 0] = 0x4B
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x03
				bodyBytes[cursor + 3] = keyP["fresh_air"]
				bodyBytes[cursor + 4] = keyP["fresh_air_fan_speed"]
				bodyBytes[cursor + 5] = 0xFF
				cursor = cursor + 6
			end
			if(keyP["parent_control"] ~= nil) then
				bodyBytes[cursor + 0] = 0x51
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x05
				bodyBytes[cursor + 3] = keyP["parent_control"]
				bodyBytes[cursor + 4] = keyP["parent_control_temp_up"]
				bodyBytes[cursor + 5] = keyP["parent_control_temp_down"]
				bodyBytes[cursor + 6] = 0xFF
				bodyBytes[cursor + 7] = 0xFF
				cursor = cursor + 8
			end
			if(keyP["buzzerValue"] ~= nil) then
                bodyBytes[cursor + 0] = 0x1A
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x01
                bodyBytes[cursor + 3] = 0x00
                if(keyP["buzzerValue"] == 0x40) then
                    bodyBytes[cursor + 3] = 0x01
                end
                cursor = cursor + 4
                bodyBytes[1] = keyP["propertyNumber"] + 1
            end
			if(keyP["wind_swing_ud_angle"] ~= nil) then
				bodyBytes[cursor + 0] = 0x09
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["wind_swing_ud_angle"]
				cursor = cursor + 4
			end
			if(keyP["wind_swing_lr_angle"] ~= nil) then
				bodyBytes[cursor + 0] = 0x0A
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["wind_swing_lr_angle"]
				cursor = cursor + 4
			end
			if(keyP["nobody_energy_save"] ~= nil) then
                bodyBytes[cursor + 0] = 0x30
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x06
                bodyBytes[cursor + 3] = keyP["nobody_energy_save"]
                bodyBytes[cursor + 4] = 0x00
                bodyBytes[cursor + 5] = 0x00
                bodyBytes[cursor + 6] = 0x00
                bodyBytes[cursor + 7] = 0x00
                bodyBytes[cursor + 8] = 0x00
                cursor = cursor + 9 
                bodyBytes[1] = keyP["propertyNumber"] + 1
            end
			if(keyP["prevent_straight_wind_lr"] ~= nil) then
                bodyBytes[cursor + 0] = 0x58
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x01
                bodyBytes[cursor + 3] = keyP["prevent_straight_wind_lr"]
                cursor = cursor + 4
                bodyBytes[1] = keyP["propertyNumber"] + 1
            end
			if(keyP["water_pump"] ~= nil) then
                bodyBytes[cursor + 0] = 0x50
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x01
                bodyBytes[cursor + 3] = keyP["water_pump"]
                cursor = cursor + 4
                bodyBytes[1] = keyP["propertyNumber"] + 1
            end
			if(keyP["intelligent_control"] ~= nil) then
                bodyBytes[cursor + 0] = 0x31
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x01
                bodyBytes[cursor + 3] = keyP["intelligent_control"]
                cursor = cursor + 4
                bodyBytes[1] = keyP["propertyNumber"] + 1
            end
			if(keyP["volume_control"] ~= nil) then
                bodyBytes[cursor + 0] = 0x24
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x04
                bodyBytes[cursor + 3] = 0x02
                bodyBytes[cursor + 4] = keyP["volume_control"]
                bodyBytes[cursor + 5] = 0xFF
                bodyBytes[cursor + 6] = 0xFF
                cursor = cursor + 7
                bodyBytes[1] = keyP["propertyNumber"] + 1
            end
			if(keyP["voice_control_new"] ~= nil) then
				bodyBytes[cursor + 0] = 0x20
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x14
				bodyBytes[cursor + 3] = keyP["voice_control_new"]
				bodyBytes[cursor + 4] = 0xFF
				bodyBytes[cursor + 5] = 0xFF
				bodyBytes[cursor + 6] = 0xFF
				bodyBytes[cursor + 7] = 0xFF
				bodyBytes[cursor + 8] = 0xFF
				bodyBytes[cursor + 9] = 0xFF
				bodyBytes[cursor + 10] = 0xFF
				bodyBytes[cursor + 11] = 0xFF
				bodyBytes[cursor + 12] = 0xFF
				bodyBytes[cursor + 13] = 0xFF
				bodyBytes[cursor + 14] = 0xFF
				bodyBytes[cursor + 15] = 0xFF
				bodyBytes[cursor + 16] = 0xFF
				bodyBytes[cursor + 17] = 0xFF
				bodyBytes[cursor + 18] = 0xFF
				bodyBytes[cursor + 19] = 0xFF
				bodyBytes[cursor + 20] = 0xFF
				bodyBytes[cursor + 21] = 0xFF
				bodyBytes[cursor + 22] = 0xFF
				cursor = cursor + 23
			end
			if(keyP["cool_temp_down"] ~= nil or keyP["cool_temp_up"] ~= nil or keyP["auto_temp_down"] ~= nil or keyP["auto_temp_up"] ~= nil or keyP["heat_temp_down"] ~= nil or keyP["heat_temp_up"] ~= nil) then
			    bodyBytes[cursor + 0] = 0x25
				bodyBytes[cursor + 1] = 0x02
				bodyBytes[cursor + 2] = 0x07
				bodyBytes[cursor + 3] = keyP["cool_temp_down"]
				bodyBytes[cursor + 4] = keyP["cool_temp_up"]
				bodyBytes[cursor + 5] = keyP["auto_temp_down"]
				bodyBytes[cursor + 6] = keyP["auto_temp_up"]
				bodyBytes[cursor + 7] = keyP["heat_temp_down"]
				bodyBytes[cursor + 8] = keyP["heat_temp_up"]
				bodyBytes[cursor + 9] = 0x00
				cursor = cursor + 10
			end
			if(keyP["remote_control_lock"] ~= nil) then
				bodyBytes[cursor + 0] = 0x27
				bodyBytes[cursor + 1] = 0x02
				bodyBytes[cursor + 2] = 0x02
				bodyBytes[cursor + 3] = keyP["remote_control_lock"]
				bodyBytes[cursor + 4] = keyP["remote_control_lock_control"]
				cursor = cursor + 5
			end
			if(keyP["operating_time"] ~= nil) then
				bodyBytes[cursor + 0] = 0x28
				bodyBytes[cursor + 1] = 0x02
				bodyBytes[cursor + 2] = 0x03
				bodyBytes[cursor + 3] = bit.band(keyP["operating_time"], 0xff)
				bodyBytes[cursor + 4] = bit.band(bit.rshift(keyP["operating_time"], 8), 0xff)
				bodyBytes[cursor + 5] = bit.band(bit.rshift(keyP["operating_time"], 16), 0xff)
				cursor = cursor + 6
			end
			if(keyP["jet_cool"] ~= nil) then
                bodyBytes[cursor + 0] = 0x67
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x01
                bodyBytes[cursor + 3] = keyP["jet_cool"]
                cursor = cursor + 4
            end
			if(keyP["rate_select"] ~= nil) then
                bodyBytes[cursor + 0] = 0x48
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x01
                bodyBytes[cursor + 3] = keyP["rate_select"]
                cursor = cursor + 4
            end
			if(keyP["main_strip_control"] ~= nil) then
                bodyBytes[cursor + 0] = 0x30
                bodyBytes[cursor + 1] = 0x02
                bodyBytes[cursor + 2] = 0x04
                bodyBytes[cursor + 3] = keyP["main_horizontal_guide_strip_1"]
				bodyBytes[cursor + 4] = keyP["main_horizontal_guide_strip_2"]
				bodyBytes[cursor + 5] = keyP["main_horizontal_guide_strip_3"]
				bodyBytes[cursor + 6] = keyP["main_horizontal_guide_strip_4"]
                cursor = cursor + 7
            end
			if(keyP["sup_strip_control"] ~= nil) then
                bodyBytes[cursor + 0] = 0x31
                bodyBytes[cursor + 1] = 0x02
                bodyBytes[cursor + 2] = 0x04
                bodyBytes[cursor + 3] = keyP["sup_horizontal_guide_strip_1"]
				bodyBytes[cursor + 4] = keyP["sup_horizontal_guide_strip_2"]
				bodyBytes[cursor + 5] = keyP["sup_horizontal_guide_strip_3"]
				bodyBytes[cursor + 6] = keyP["sup_horizontal_guide_strip_4"]
                cursor = cursor + 7
            end
			if(keyP["sound"] ~= nil) then
				bodyBytes[cursor + 0] = 0x2C
				bodyBytes[cursor + 1] = 0x02
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["sound"]
				cursor = cursor + 4
			end
			if(keyP["anion"] ~= nil) then
				bodyBytes[cursor + 0] = 0x1E
				bodyBytes[cursor + 1] = 0x02
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["anion"]
				cursor = cursor + 4
			end
			if(keyP["prevent_straight_wind_select"] ~= nil) then
				bodyBytes[cursor + 0] = 0x58
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["prevent_straight_wind_select"]
				cursor = cursor + 4
			end
			if(keyP["wind_around"] ~= nil) then
                bodyBytes[cursor + 0] = 0x59
                bodyBytes[cursor + 1] = 0x00
                bodyBytes[cursor + 2] = 0x02
                bodyBytes[cursor + 3] = keyP["wind_around"]
                bodyBytes[cursor + 4] = 0
				if(keyP["wind_around_ud"] ~= nil) then
					bodyBytes[cursor + 4] = keyP["wind_around_ud"]
				end
                cursor = cursor + 5
            end
			if(keyP["ieco_status"] == 1)then
				bodyBytes[cursor + 0] = 0xE3
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x0D
				bodyBytes[cursor + 3] = keyP["ieco_frame"]
				bodyBytes[cursor + 4] = keyP["ieco_number"]
				bodyBytes[cursor + 5] = keyP["ieco_switch"]
				bodyBytes[cursor + 6] = bit.band(keyP["ieco_target_rate"], 0xff)
				bodyBytes[cursor + 7] = bit.band(bit.rshift(keyP["ieco_target_rate"], 8), 0xff)
				bodyBytes[cursor + 8] = keyP["ieco_indoor_wind_speed_level"]
				bodyBytes[cursor + 9] = bit.band(keyP["ieco_indoor_wind_speed"], 0xff)
				bodyBytes[cursor + 10] = bit.band(bit.rshift(keyP["ieco_indoor_wind_speed"], 8), 0xff)
				bodyBytes[cursor + 11] = keyP["ieco_outdoor_wind_speed_level"]
				bodyBytes[cursor + 12] = bit.band(keyP["ieco_outdoor_wind_speed"], 0xff)
				bodyBytes[cursor + 13] = bit.band(bit.rshift(keyP["ieco_outdoor_wind_speed"], 8), 0xff)
				bodyBytes[cursor + 14] = bit.band(keyP["ieco_expansion_valve"], 0xff)
				bodyBytes[cursor + 15] = bit.band(bit.rshift(keyP["ieco_expansion_valve"], 8), 0xff)
				cursor = cursor + 16
			end
			if(keyP["mito_cool"] ~= nil) then
				bodyBytes[cursor + 0] = 0x8D
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["mito_cool"] * 2 + 50
				cursor = cursor + 4
			end
			if(keyP["mito_heat"] ~= nil) then
				bodyBytes[cursor + 0] = 0x8E
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["mito_heat"] * 2 + 50
				cursor = cursor + 4
			end
			if(keyP["dr_time_hour"] ~= nil or keyP["dr_time_min"] ~= nil) then
				bodyBytes[cursor + 0] = 0x8F
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x02
				bodyBytes[cursor + 3] = keyP["dr_time_min"]
				bodyBytes[cursor + 4] = keyP["dr_time_hour"]
				cursor = cursor + 5
			end
			if(keyP["cvp"] ~= nil) then
				bodyBytes[cursor + 0] = 0x98
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["cvp"]
				cursor = cursor + 4
			end
			if(keyP["new_wind_sense"] ~= nil) then
				bodyBytes[cursor + 0] = 0xAA
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["new_wind_sense"]
				cursor = cursor + 4
			end
			if(keyP["comfort"] ~= nil) then
				bodyBytes[cursor + 0] = 0xAD
				bodyBytes[cursor + 1] = 0x00
				bodyBytes[cursor + 2] = 0x01
				bodyBytes[cursor + 3] = keyP["comfort"]
				cursor = cursor + 4
			end
		
	        math.randomseed(tostring(os.time()*#bodyBytes):reverse():sub(1, 7))
			math.random()
			bodyBytes[cursor] = math.random(1, 254)
		
			bodyBytes[cursor + 1] = crc8_854(bodyBytes, 0, cursor)
		end
        --构造消息部分
        infoM = getTotalMsg(bodyBytes,keyB["BYTE_CONTROL_REQUEST"])
    end
	keyP["propertyNumber"] = 0
	keyP["prevent_super_cool"] = nil 
	keyP["prevent_straight_wind"] = nil 
	keyP["auto_prevent_straight_wind"] = nil 
	keyP["wind_straight"] = nil 
	keyP["wind_avoid"] = nil 
	keyP["yb_wind_avoid"] = nil
	keyP["intelligent_wind"] = nil 
	keyP["self_clean"] = nil 
	keyP["no_wind_sense"] = nil 
	keyP["no_wind_sense_level"] = nil
	keyP["child_prevent_cold_wind"] = nil 
	keyP["little_angel"] = nil 
	keyP["cool_hot_sense"] = nil 
	keyP["gentle_wind_sense"] = nil 
	keyP["security"] = nil
	keyP["even_wind"] = nil
	keyP["single_tuyere"] = nil
	keyP["extreme_wind"] = nil
	keyP["extreme_wind_level"] = nil
	keyP["voice_control"] = nil
	keyP["pre_cool_hot"] = nil
	keyP["water_washing"] = nil
	keyP["fresh_air"] = nil
	keyP["fa_prevent_straight_wind"] = nil
	keyP["parent_control"] = nil
	keyP["parent_control_temp_up"] = nil
	keyP["parent_control_temp_down"] = nil
	keyP["nobody_energy_save"] = nil
	keyP["filter_value"] = nil
	keyP["filter_level"] = nil
	keyP["prevent_straight_wind_lr"] = nil
	keyP["pm25_value"] = nil
	keyP["water_pump"] = nil
	keyP["intelligent_control"] = nil
	keyP["wind_swing_ud_angle"] = nil
	keyP["wind_swing_lr_angle"] = nil
	keyP["volume_control"] = nil
	keyP["voice_control_new"] = nil
	keyP["face_register"] = nil
	keyP["cool_temp_up"] = nil
	keyP["cool_temp_down"] = nil
	keyP["auto_temp_up"] = nil
	keyP["auto_temp_down"] = nil
	keyP["heat_temp_up"] = nil
	keyP["heat_temp_down"] = nil
	keyP["remote_control_lock"] = nil
	keyP["remote_control_lock_control"] = nil
	keyP["operating_time"] = nil
	keyP["fa_no_wind_sense"] = nil
	keyP["indoor_humidity"] = nil
	keyP["prevent_straight_wind_flag"] = nil
	keyP["rate_select"] = nil
	keyP["sound"] = nil
	keyP["b5_parent_control"] = nil
	keyP["ieco_status"] = nil
	keyP["wind_around"] = nil
	keyP["wind_around_ud"] = nil
	keyP["b5_wind_around"] = nil
	keyP["prevent_straight_wind_select"] = nil
	keyP["b5_prevent_straight_wind_select"] = nil
	keyP["cvp"] = nil
	keyP["b5_cvp"] = nil
	keyP["new_wind_sense"] = nil
	keyP["b5_new_wind_sense"] = nil
	keyP["comfort"] = nil
	propertyPre = nil
	
    --table 转换成 string 之后返回
    local ret = table2string(infoM)
    ret = string2hexstring(ret)
	return ret
end


--二进制转json
function dataToJson(jsonCmd)
	
    init_keyP()
	if (not jsonCmd) then
        return nil
    end
    
    local json = decode(jsonCmd)
    local deviceinfo = json["deviceinfo"]
    deviceSubType = deviceinfo["deviceSubType"]
    local deviceSN=json["deviceinfo"]["deviceSN"]
    if deviceSN~=nil then
        deviceSN8=string.sub(deviceSN,13,17)
    end

    local status = json["status"]
    if (status) then
        jsonToModel(status,"status")
    end

    local binData = json["msg"]["data"]
    local info = {}
    local msgBytes = {}
    local bodyBytes = {}
    local msgLength = 0
    local bodyLength = 0
    
    info = string2table(binData)

    local streams = {}
    dataType=info[10];
	streams["protocolType"] = info[9]

    for i = 1, #info do
        msgBytes[i - 1] = info[i]
    end
    
    msgLength = msgBytes[1]
    bodyLength = msgLength - keyB["BYTE_PROTOCOL_LENGTH"] - 1

	streams["b5_next_frame"] = info[msgLength - 2]
    
	--获取 body 部分
    for i = 0, bodyLength do
        bodyBytes[i] = msgBytes[i + keyB["BYTE_PROTOCOL_LENGTH"]]
    end
    
    --将二进制状态解析为属性值
    binToModel(bodyBytes,deviceSN8)
    
    --将属性值转换为最终 table
	
    --版本
    streams[keyT["KEY_VERSION"]] = keyV["VALUE_VERSION"]

    if (keyP["propertyNumber"] == 0) then
    --处理常规协议
	
	--电源
    if (keyP["powerValue"] ~= nil) then
		if (keyP["powerValue"] == keyB["BYTE_POWER_ON"]) then
			streams[keyT["KEY_POWER"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["powerValue"] == keyB["BYTE_POWER_OFF"]) then
			streams[keyT["KEY_POWER"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
    --模式
    if (keyP["modeValue"] ~= nil) then
		if (keyP["modeValue"] == keyB["BYTE_MODE_HEAT"]) then
			streams[keyT["KEY_MODE"]] = keyV["VALUE_MODE_HEAT"]
		elseif (keyP["modeValue"] == keyB["BYTE_MODE_COOL"]) then
			streams[keyT["KEY_MODE"]] = keyV["VALUE_MODE_COOL"]
		elseif (keyP["modeValue"] == keyB["BYTE_MODE_AUTO"]) then
			streams[keyT["KEY_MODE"]] = keyV["VALUE_MODE_AUTO"]
		elseif (keyP["modeValue"] == keyB["BYTE_MODE_DRY"]) then
			streams[keyT["KEY_MODE"]] = keyV["VALUE_MODE_DRY"]
			if(keyP["smartDryValue"] ~= nil and keyP["smartDryValue"] >= 30 and  keyP["smartDryValue"] <= 101) then
				streams[keyT["KEY_SMART_DRY"]] = keyP["smartDryValue"]
			end
		elseif (keyP["modeValue"] == keyB["BYTE_MODE_FAN"]) then
			streams[keyT["KEY_MODE"]] = keyV["VALUE_MODE_FAN"]
		elseif (keyP["modeValue"] == keyB["BYTE_MODE_SMART_DRY"]) then
			streams[keyT["KEY_MODE"]] = keyV["VALUE_MODE_SMART_DRY"]
			if(keyP["smartDryValue"] ~= nil and keyP["smartDryValue"] >= 30 and  keyP["smartDryValue"] <= 101) then
				streams[keyT["KEY_SMART_DRY"]] = keyP["smartDryValue"]
			end
		end
	end
    
    --净化
	if (keyP["purifierValue"] ~= nil) then
		if (keyP["purifierValue"] == keyB["BYTE_PURIFIER_ON"]) then
			streams[keyT["KEY_PURIFIER"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["purifierValue"] == keyB["BYTE_PURIFIER_OFF"]) then
			streams[keyT["KEY_PURIFIER"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
    --ECO
	if (keyP["ecoValue"] ~= nil) then
		if (keyP["ecoValue"] == keyB["BYTE_ECO_ON"]) then
			streams[keyT["KEY_ECO"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["ecoValue"] == keyB["BYTE_ECO_OFF"]) then
			streams[keyT["KEY_ECO"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
    --干燥
	if ((keyP["dryValue"] ~= nil) and (keyP["modeValue"] ~= nil)) then
		if (keyP["dryValue"] == keyB["BYTE_DRY_ON"]) then
			streams[keyT["KEY_DRY"]] = keyV["VALUE_FUNCTION_ON"]
		else
			streams[keyT["KEY_DRY"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
    --风速
	if (keyP["fanspeedValue"] ~= nil) then
		streams[keyT["KEY_FANSPEED"]] = keyP["fanspeedValue"]
	end

    --室外温度
	if ((keyP["outdoorTemperatureValue"] ~= nil) and (keyP["smallOutdoorTemperatureValue"] ~= nil)) then
		streams[keyV["VALUE_OUTDOOR_TEMPERATURE"]] = keyP["outdoorTemperatureValue"] +keyP["smallOutdoorTemperatureValue"]/10
    end

    --室内温度
	if ((keyP["indoorTemperatureValue"] ~= nil) and (keyP["smallIndoorTemperatureValue"] ~= nil)) then
		streams[keyV["VALUE_INDOOR_TEMPERATURE"]] = keyP["indoorTemperatureValue"]+keyP["smallIndoorTemperatureValue"]/10
    end

    --上下扫风
	if (keyP["swingUDValue"] ~= nil) then
		if (keyP["swingUDValue"] == keyB["BYTE_SWING_UD_ON"]) then
			streams[keyT["KEY_SWING_UD"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["swingUDValue"] == keyB["BYTE_SWING_UD_OFF"]) then
			streams[keyT["KEY_SWING_UD"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
    --左右扫风
	if (keyP["swingLRValue"] ~= nil) then
		if (keyP["swingLRValue"] == keyB["BYTE_SWING_LR_ON"]) then
			streams[keyT["KEY_SWING_LR"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["swingLRValue"] == keyB["BYTE_SWING_LR_OFF"]) then
			streams[keyT["KEY_SWING_LR"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
	
	--下左右扫风
	if (keyP["swingLRUnderSwitch"] == keyB["BYTE_SWING_LR_UNDER_ENABLE"]) then
        if (keyP["swingLRValueUnder"] == keyB["BYTE_SWING_LR_UNDER_ON"]) then
            streams[keyT["KEY_SWING_LR_UNDER"]] = keyV["VALUE_FUNCTION_ON"]
        elseif (keyP["swingLRValueUnder"] == keyB["BYTE_SWING_LR_UNDER_OFF"]) then
            streams[keyT["KEY_SWING_LR_UNDER"]] = keyV["VALUE_FUNCTION_OFF"]
        end
    else
        streams[keyT["KEY_SWING_LR_UNDER"]] = streams[keyT["KEY_SWING_LR"]]
    end
    
    --电辅热
	if ((keyP["PTCValue"] ~= nil) and (keyP["modeValue"] ~= nil )) then
		if (keyP["PTCValue"] == keyB["BYTE_PTC_ON"]) and ((keyP["modeValue"] == keyB["BYTE_MODE_AUTO"]) or (keyP["modeValue"] == keyB["BYTE_MODE_HEAT"])) then
			streams[keyT["KEY_PTC"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["PTCValue"] == keyB["BYTE_PTC_OFF"]) then
			streams[keyT["KEY_PTC"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
	
	--单独电辅热
	if (keyP["independent_ptc"] ~= nil) then
		if (keyP["independent_ptc"] == 0x01) then
			streams["independent_ptc"] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["independent_ptc"] == 0x00) then
			streams["independent_ptc"] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
	
	--定时开
	if (keyP["openTimerSwitch"] ~= nil) then
		if (keyP["openTimerSwitch"] == keyB["BYTE_START_TIMER_SWITCH_ON"]) then
			streams[keyT["KEY_TIME_ON"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["openTimerSwitch"] == keyB["BYTE_START_TIMER_SWITCH_OFF"]) then
				streams[keyT["KEY_TIME_ON"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end

	--定时关
	if (keyP["closeTimerSwitch"] ~= nil) then
		if (keyP["closeTimerSwitch"] == keyB["BYTE_CLOSE_TIMER_SWITCH_ON"]) then
			streams[keyT["KEY_TIME_OFF"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["closeTimerSwitch"] == keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]) then
			streams[keyT["KEY_TIME_OFF"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
    --定时关机时间
	if (keyP["closeTimerSwitch"] ~= nil) then
		if (keyP["closeTimerSwitch"] == keyB["BYTE_CLOSE_TIMER_SWITCH_OFF"]) then
			streams[keyT["KEY_CLOSE_TIME"]] = 0
		else
			streams[keyT["KEY_CLOSE_TIME"]] = keyP["closeTime"]
		end
	end

    --定时开机时间
	if (keyP["openTimerSwitch"] ~= nil) then
		if (keyP["openTimerSwitch"] == keyB["BYTE_START_TIMER_SWITCH_OFF"]) then
			streams[keyT["KEY_OPEN_TIME"]] = 0
		else
			streams[keyT["KEY_OPEN_TIME"]] = keyP["openTime"]
		end
	end
    
    --本次开机运行时间
	if(keyP["currentWorkTime"] ~= nil) then
		streams[keyT["KEY_CURRENT_WORK_TIME"]] = keyP["currentWorkTime"]
	end

	--强劲
	if (keyP["strongWindValue"] ~= nil) then
		if (keyP["strongWindValue"] == keyB["BYTE_STRONG_WIND_ON"]) then
			streams[keyT["KEY_STRONG_WIND"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["strongWindValue"] == keyB["BYTE_STRONG_WIND_OFF"]) then
			streams[keyT["KEY_STRONG_WIND"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end

	--tubro(AB、AE强劲)
	if (keyP["tubroValue"] ~= nil) then
		if (keyP["tubroValue"] == 0x02) then
			streams[keyT["KEY_TUBRO"]] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["tubroValue"] == 0x00) then
			streams[keyT["KEY_TUBRO"]] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
	
	--省电
	if (keyP["power_saving"] ~= nil) then
		if (keyP["power_saving"] == 0x08) then
			streams["power_saving"] = keyV["VALUE_FUNCTION_ON"]
		elseif (keyP["power_saving"] == 0x00) then
			streams["power_saving"] = keyV["VALUE_FUNCTION_OFF"]
		end
	end
    
	--温度
	if(keyP["temperature"] ~= nil) then
		streams[keyT["KEY_TEMPERATURE"] ]= keyP["temperature"]
	end

	--温度小数位
	if(keyP["smallTemperature"] ~= nil) then
		if(keyP["smallTemperature"] == 0x01) then
			streams["small_temperature"] = 0.5
		else
			streams["small_temperature"] = 0
		end
	end

	--华氏温度处理
	if(keyP["temperature_unit"] ~= nil ) then
		if(keyP["temperature_unit"] == 1) then
			streams[keyT["KEY_TEMPERATURE"] ]= convert_to_F(keyP["temperature"] + streams["small_temperature"])
			streams["small_temperature"] = 0
			streams["temperature_unit"] = 1
		else
			streams["temperature_unit"] = 0
		end
	end
	if(keyP["errorCode"] ~= nil)then
		streams[keyT["KEY_ERROR_CODE"]]=keyP["errorCode"]
	end
	
	--是否踢被子
	if(keyP["kickQuilt"] ~= nil) then
		if(keyP["kickQuilt"] == 0x00) then
			streams["kick_quilt"] = "off"
		elseif(keyP["kickQuilt"] == 0x01) then
			streams["kick_quilt"] = "on"
		end
	end
	
	--舒省
	if(keyP["comfortPowerSave"] ~= nil) then
		if(keyP["comfortPowerSave"] == 0x00) then
			streams["comfort_power_save"] = "off"
		elseif(keyP["comfortPowerSave"] == 0x01) then
			streams["comfort_power_save"] = "on"
		end
	end

	--无风感
	if(keyP["no_wind_sense"] ~= nil) then
		streams["no_wind_sense"] = keyP["no_wind_sense"]
	end
	
	--远近无风感
	if(keyP["fa_no_wind_sense"] ~= nil) then
		streams["fa_no_wind_sense"] = keyP["fa_no_wind_sense"]
	end
	
	--无风感等级
	if(keyP["no_wind_sense_level"] ~= nil) then
	    streams["no_wind_sense_level"] = keyP["no_wind_sense_level"]
	end

		
	--防着凉
	if(keyP["preventCold"] ~= nil) then
		if(keyP["preventCold"] == 0x00) then
			streams["prevent_cold"] = "off"
		elseif(keyP["preventCold"] == 0x01) then
			streams["prevent_cold"] = "on"
		end
	end
		
	--8度制热
	if(keyP["degree8_heat"] ~= nil) then
	    streams["degree8_heat"] = keyP["degree8_heat"]
	end
	
	--舒睡
	if(keyP["comfortableSleepValue"] ~= nil) then
	    if(keyP["comfortableSleepValue"] == 0x00 and keyP["comfortableSleepSwitch"] == 0x00) then
		    streams["comfort_sleep"] = "off"
		elseif(keyP["comfortableSleepValue"] == 0x03 and keyP["comfortableSleepSwitch"] == 0x40) then
		    streams["comfort_sleep"] = "on"
		end
	end
	
	--屏显状态
	if(keyP["screenDisplayNowValue"] ~= nil) then
		if(keyP["screenDisplayNowValue"] == 0x07) then
			streams["screen_display_now"] = "off"
		else
			streams["screen_display_now"] = "on"
		end
	end
	
	--自然风
	if(keyP["naturalWind"] ~= nil) then
		if(keyP["naturalWind"] == 0x02) then
			streams["natural_wind"] = "on"
		elseif(keyP["naturalWind"] == 0x00) then
			streams["natural_wind"] = "off"
		end
	end
	
	--pmv
	if(keyP["pmv"] ~= nil) then
		streams["pmv"] = keyP["pmv"]
	end
	
	--新风滤网总时长
	if(keyP["fresh_filter_time_total"] ~= nil) then
		streams["fresh_filter_time_total"] = keyP["fresh_filter_time_total"]
	end
	
	--新风滤网使用时长
	if(keyP["fresh_filter_time_use"] ~= nil) then
		streams["fresh_filter_time_use"] = keyP["fresh_filter_time_use"]
	end
	
	--新风滤网超时标志
	if(keyP["fresh_filter_timeout"] ~= nil) then
		streams["fresh_filter_timeout"] = keyP["fresh_filter_timeout"]
	end
	--新风滤网超时标志AE2
	if(keyP["fresh_filter_timeout_ae2"] ~= nil) then
		streams["fresh_filter_timeout_ae2"] = keyP["fresh_filter_timeout_ae2"]
	end
	
	if(keyP["real_time_power"] ~= nil) then
		streams["real_time_power"] = keyP["real_time_power"]
	end
	if(keyP["real_time_power_10"] ~= nil) then
		streams["real_time_power_10"] = keyP["real_time_power_10"]
	end
	if(keyP["current_humidity"] ~= nil) then
		streams["current_humidity"] = keyP["current_humidity"]
	end
	if(keyP["sleep_status"] ~= nil) then
		streams["sleep_status"] = keyP["sleep_status"]
	end
	if(keyP["machine_type"] ~= nil) then
		streams["machine_type"] = keyP["machine_type"]
	end
	if(keyP["product_type"] ~= nil) then
		streams["product_type"] = keyP["product_type"]
	end

	else
		--新协议，变长属性控制协议
		if(keyP["prevent_super_cool"] ~= nil) then
			if(keyP["prevent_super_cool"] == 0x00) then
				streams["prevent_super_cool"] = "off"
			elseif(keyP["prevent_super_cool"] == 0x01) then
				streams["prevent_super_cool"] = "on"
			end
		end
		if(keyP["prevent_straight_wind"] ~= nil) then
			streams["prevent_straight_wind"] = keyP["prevent_straight_wind"]
		end
		if(keyP["prevent_straight_wind_flag"] ~= nil) then
			streams["prevent_straight_wind_flag"] = keyP["prevent_straight_wind_flag"]
		end
		if(keyP["fa_no_wind_sense"] ~= nil) then
			streams["fa_no_wind_sense"] = keyP["fa_no_wind_sense"]
		end
		if(keyP["auto_prevent_straight_wind"] ~= nil) then
			if(keyP["auto_prevent_straight_wind"] == 0x00) then
				streams["auto_prevent_straight_wind"] = "off"
			elseif(keyP["auto_prevent_straight_wind"] == 0x01) then
				streams["auto_prevent_straight_wind"] = "on"
			end
		end
		if(keyP["self_clean"] ~= nil) then
			if(keyP["self_clean"] == 0x00) then
				streams["self_clean"] = "off"
			elseif(keyP["self_clean"] == 0x01) then
				streams["self_clean"] = "on"
			end
		end
		if(keyP["wind_straight"] ~= nil) then
			if(keyP["wind_straight"] == 0x00) then
				streams["wind_straight"] = "off"
			elseif(keyP["wind_straight"] == 0x01) then
				streams["wind_straight"] = "on"
			end
		end
		if(keyP["yb_wind_avoid"] ~= nil) then
			if(keyP["yb_wind_avoid"] == 0x00) then
				streams["yb_wind_avoid"] = "off"
			elseif(keyP["yb_wind_avoid"] == 0x02) then
				streams["yb_wind_avoid"] = "on"
			end
		end
		if(keyP["wind_avoid"] ~= nil) then
			if(keyP["wind_avoid"] == 0x00) then
				streams["wind_avoid"] = "off"
			elseif(keyP["wind_avoid"] == 0x01 or keyP["wind_avoid"] == 0x02) then
				streams["wind_avoid"] = "on"
			end
		end
		if(keyP["intelligent_wind"] ~= nil) then
			if(keyP["intelligent_wind"] == 0x00) then
				streams["intelligent_wind"] = "off"
			elseif(keyP["intelligent_wind"] == 0x01) then
				streams["intelligent_wind"] = "on"
			end
		end
		if(keyP["child_prevent_cold_wind"] ~= nil) then
			if(keyP["child_prevent_cold_wind"] == 0x00) then
				streams["child_prevent_cold_wind"] = "off"
			elseif(keyP["child_prevent_cold_wind"] == 0x01) then
				streams["child_prevent_cold_wind"] = "on"
			end
		end
		if(keyP["no_wind_sense"] ~= nil) then
			streams["no_wind_sense"] = keyP["no_wind_sense"]
		end
		if(keyP["no_wind_sense_level"] ~= nil) then
	        streams["no_wind_sense_level"] = keyP["no_wind_sense_level"]
	    end
		if(keyP["little_angel"] ~= nil) then
			if(keyP["little_angel"] == 0x00) then
				streams["little_angel"] = "off"
			elseif(keyP["little_angel"] == 0x01) then
				streams["little_angel"] = "on"
			end
		end
		if(keyP["cool_hot_sense"] ~= nil) then
			if(keyP["cool_hot_sense"] == 0x00) then
				streams["cool_hot_sense"] = "off"
			elseif(keyP["cool_hot_sense"] == 0x01) then
				streams["cool_hot_sense"] = "on"
			end
		end
		if(keyP["gentle_wind_sense"] ~= nil) then
			if(keyP["gentle_wind_sense"] == 0x01) then
				streams["gentle_wind_sense"] = "off"
			elseif(keyP["gentle_wind_sense"] == 0x03) then
				streams["gentle_wind_sense"] = "on"
			end
		end
		if(keyP["security"] ~= nil) then 
			if(keyP["security"] == 0x00) then 
			streams["security"] = "off" 
			elseif(keyP["security"] == 0x01) then 
			streams["security"] = "on"
			end
		end
		if(keyP["even_wind"] ~= nil) then 
			if(keyP["even_wind"] == 0x00) then 
			streams["even_wind"] = "off" 
			elseif(keyP["even_wind"] == 0x01) then 
			streams["even_wind"] = "on"
			end
		end
		if(keyP["single_tuyere"] ~= nil) then 
			if(keyP["single_tuyere"] == 0x00) then 
			streams["single_tuyere"] = "off" 
			elseif(keyP["single_tuyere"] == 0x01) then 
			streams["single_tuyere"] = "on"
			end
		end
		if(keyP["extreme_wind"] ~= nil) then 
			if(keyP["extreme_wind"] == 0x00) then 
			streams["extreme_wind"] = "off" 
			elseif(keyP["extreme_wind"] == 0x01) then 
			streams["extreme_wind"] = "on"
			end
			
			streams["extreme_wind_level"] =  keyP["extreme_wind_level"]
		end
		if(keyP["wind_swing_ud_angle"] ~= nil) then 
			streams["wind_swing_ud_angle"] = keyP["wind_swing_ud_angle"]
		end
		if(keyP["wind_swing_lr_angle"] ~= nil) then 
			streams["wind_swing_lr_angle"] = keyP["wind_swing_lr_angle"]
		end
		if(keyP["voice_control"] ~= nil) then 
			if(keyP["voice_control"] == 0x00) then 
			streams["voice_control"] = "off" 
			elseif(keyP["voice_control"] == 0x03) then 
			streams["voice_control"] = "on"
			end
		end
		if(keyP["pre_cool_hot"] ~= nil) then 
			if(keyP["pre_cool_hot"] == 0x00) then 
			streams["pre_cool_hot"] = "off" 
			elseif(keyP["pre_cool_hot"] == 0x01) then 
			streams["pre_cool_hot"] = "on"
			end
		end
		if(keyP["water_washing"] ~= nil) then 
			if(keyP["water_washing"] == 0x01) then 
			streams["water_washing"] = "on" 
			elseif(keyP["water_washing"] == 0x00) then 
			streams["water_washing"] = "off"
			end
			
			streams["water_washing_manual"] = keyP["water_washing_manual"]
			streams["water_washing_time"] = keyP["water_washing_time"]
			streams["water_washing_stage"] = keyP["water_washing_stage"]
		end
		if(keyP["fresh_air"] ~= nil) then 
			if(keyP["fresh_air"] == 0x00) then 
			streams["fresh_air"] = "off" 
			elseif(keyP["fresh_air"] == 0x01) then 
			streams["fresh_air"] = "on"
			end
			
			streams["fresh_air_fan_speed"] = keyP["fresh_air_fan_speed"]
			streams["fresh_air_temp"] = keyP["fresh_air_temp"]
		end
		if(keyP["parent_control"] ~= nil) then 
			if(keyP["parent_control"] == 0x00) then 
			streams["parent_control"] = "off" 
			elseif(keyP["parent_control"] == 0x01) then 
			streams["parent_control"] = "on"
			end
			
			streams["parent_control_temp_up"] = keyP["parent_control_temp_up"]
			streams["parent_control_temp_down"] = keyP["parent_control_temp_down"]
		end
		if(keyP["nobody_energy_save"] ~= nil) then 
			if(keyP["nobody_energy_save"] == 0x00) then 
			streams["nobody_energy_save"] = "off" 
			elseif(keyP["nobody_energy_save"] == 0x01) then 
			streams["nobody_energy_save"] = "on"
			end
		end
		if(keyP["filter_value"] ~= nil) then 
			streams["filter_value"] = keyP["filter_value"]
			streams["filter_level"] = keyP["filter_level"]
		end
		if(keyP["prevent_straight_wind_lr"] ~= nil) then 
			streams["prevent_straight_wind_lr"] = keyP["prevent_straight_wind_lr"]
		end
		if(keyP["pm25_value"] ~= nil) then 
			streams["pm25_value"] = keyP["pm25_value"]
		end
		if(keyP["water_pump"] ~= nil) then 
			if(keyP["water_pump"] == 0x00) then 
			streams["water_pump"] = "off" 
			elseif(keyP["water_pump"] == 0x01) then 
			streams["water_pump"] = "on"
			end
		end
		if(keyP["intelligent_control"] ~= nil) then 
			if(keyP["intelligent_control"] == 0x00) then 
			streams["intelligent_control"] = "off" 
			elseif(keyP["intelligent_control"] == 0x01) then 
			streams["intelligent_control"] = "on"
			end
		end
		if(keyP["volume_control"] ~= nil) then 
			streams["volume_control"] = keyP["volume_control"]
		end
		if(keyP["voice_control_new"] ~= nil) then 
			streams["voice_control_new"] = keyP["voice_control_new"]
		end
		if(keyP["face_register"] ~= nil) then 
			streams["face_register"] = keyP["face_register"]
		end
		if(keyP["cool_temp_up"] ~= nil) then 
	        streams["cool_temp_up"] = keyP["cool_temp_up"]
		end
		if(keyP["cool_temp_down"] ~= nil) then 
			streams["cool_temp_down"] = keyP["cool_temp_down"]
		end
		if(keyP["auto_temp_up"] ~= nil) then 
			streams["auto_temp_up"] = keyP["auto_temp_up"]
		end
		if(keyP["auto_temp_down"] ~= nil) then 
			streams["auto_temp_down"] = keyP["auto_temp_down"]
		end
		if(keyP["heat_temp_up"] ~= nil) then 
			streams["heat_temp_up"] = keyP["heat_temp_up"]
		end
		if(keyP["heat_temp_down"] ~= nil) then 
			streams["heat_temp_down"] = keyP["heat_temp_down"]
		end
		if(keyP["remote_control_lock"] ~= nil) then
		    streams["remote_control_lock"] = keyP["remote_control_lock"]
		end
		if(keyP["remote_control_lock_control"] ~= nil) then
		     streams["remote_control_lock_control"] = keyP["remote_control_lock_control"]
		end
		if(keyP["operating_time"] ~= nil) then
		    streams["operating_time"] = keyP["operating_time"]
		end
		if(keyP["indoor_humidity"] ~= nil) then
		    streams["indoor_humidity"] = keyP["indoor_humidity"]
		end
		if(keyP["rate_select"] ~= nil) then
		    streams["rate_select"] = keyP["rate_select"]
		end
		if(keyP["main_horizontal_guide_strip_2"] ~= nil) then
		    streams["main_horizontal_guide_strip_2"] = keyP["main_horizontal_guide_strip_2"]
		end
		if(keyP["main_horizontal_guide_strip_1"] ~= nil) then
		    streams["main_horizontal_guide_strip_1"] = keyP["main_horizontal_guide_strip_1"]
		end
		if(keyP["main_horizontal_guide_strip_3"] ~= nil) then
		    streams["main_horizontal_guide_strip_3"] = keyP["main_horizontal_guide_strip_3"]
		end
		if(keyP["main_horizontal_guide_strip_4"] ~= nil) then
		    streams["main_horizontal_guide_strip_4"] = keyP["main_horizontal_guide_strip_4"]
		end
		if(keyP["sup_horizontal_guide_strip_1"] ~= nil) then
		    streams["sup_horizontal_guide_strip_1"] = keyP["sup_horizontal_guide_strip_1"]
		end
		if(keyP["sup_horizontal_guide_strip_2"] ~= nil) then
		    streams["sup_horizontal_guide_strip_2"] = keyP["sup_horizontal_guide_strip_2"]
		end
		if(keyP["sup_horizontal_guide_strip_3"] ~= nil) then
		    streams["sup_horizontal_guide_strip_3"] = keyP["sup_horizontal_guide_strip_3"]
		end
		if(keyP["sup_horizontal_guide_strip_4"] ~= nil) then
		    streams["sup_horizontal_guide_strip_4"] = keyP["sup_horizontal_guide_strip_4"]
		end
		if(keyP["twins_machine"] ~= nil) then
		    streams["twins_machine"] = keyP["twins_machine"]
		end
		if(keyP["guide_strip_type"] ~= nil) then
		    streams["guide_strip_type"] = keyP["guide_strip_type"]
		end
		if(keyP["b5_mode"] ~= nil) then
		    streams["b5_mode"] = keyP["b5_mode"]
		end
		if(keyP["b5_strong_wind"] ~= nil) then
		    streams["b5_strong_wind"] = keyP["b5_strong_wind"]
		end
		if(keyP["b5_wind_speed"] ~= nil) then
		    streams["b5_wind_speed"] = keyP["b5_wind_speed"]
		end
		if(keyP["b5_humidity"] ~= nil) then
		    streams["b5_humidity"] = keyP["b5_humidity"]
		end
		if(keyP["b5_temperature_0"] ~= nil) then
		    streams["b5_temperature_0"] = keyP["b5_temperature_0"]
		end
		if(keyP["b5_temperature_1"] ~= nil) then
		    streams["b5_temperature_1"] = keyP["b5_temperature_1"]
		end
		if(keyP["b5_temperature_2"] ~= nil) then
		    streams["b5_temperature_2"] = keyP["b5_temperature_2"]
		end
		if(keyP["b5_temperature_3"] ~= nil) then
		    streams["b5_temperature_3"] = keyP["b5_temperature_3"]
		end
		if(keyP["b5_temperature_4"] ~= nil) then
		    streams["b5_temperature_4"] = keyP["b5_temperature_4"]
		end
		if(keyP["b5_temperature_5"] ~= nil) then
		    streams["b5_temperature_5"] = keyP["b5_temperature_5"]
		end
		if(keyP["b5_temperature_6"] ~= nil) then
		    streams["b5_temperature_6"] = keyP["b5_temperature_6"]
		end
		if(keyP["b5_eco"] ~= nil) then
		    streams["b5_eco"] = keyP["b5_eco"]
		end
		if(keyP["b5_filter_remind"] ~= nil) then
		    streams["b5_filter_remind"] = keyP["b5_filter_remind"]
		end
		if(keyP["b5_filter_check"] ~= nil) then
		    streams["b5_filter_check"] = keyP["b5_filter_check"]
		end
		if(keyP["b5_fahrenheit"] ~= nil) then
		    streams["b5_fahrenheit"] = keyP["b5_fahrenheit"]
		end
		if(keyP["b5_8_heat"] ~= nil) then
		    streams["b5_8_heat"] = keyP["b5_8_heat"]
		end
		if(keyP["b5_electricity"] ~= nil) then
		    streams["b5_electricity"] = keyP["b5_electricity"]
		end
		if(keyP["b5_ptc"] ~= nil) then
		    streams["b5_ptc"] = keyP["b5_ptc"]
		end
		if(keyP["b5_wind_straight"] ~= nil) then
		    streams["b5_wind_straight"] = keyP["b5_wind_straight"]
		end
		if(keyP["b5_wind_avoid"] ~= nil) then
		    streams["b5_wind_avoid"] = keyP["b5_wind_avoid"]
		end
		if(keyP["b5_wind_swing"] ~= nil) then
		    streams["b5_wind_swing"] = keyP["b5_wind_swing"]
		end
		if(keyP["b5_no_wind_sense"] ~= nil) then
		    streams["b5_no_wind_sense"] = keyP["b5_no_wind_sense"]
		end
		if(keyP["b5_screen_display"] ~= nil) then
		    streams["b5_screen_display"] = keyP["b5_screen_display"]
		end
		if(keyP["b5_anion"] ~= nil) then
		    streams["b5_anion"] = keyP["b5_anion"]
		end
		if(keyP["b5_self_clean"] ~= nil) then
		    streams["b5_self_clean"] = keyP["b5_self_clean"]
		end
		if(keyP["b5_fa_no_wind_sense"] ~= nil) then
		    streams["b5_fa_no_wind_sense"] = keyP["b5_fa_no_wind_sense"]
		end
		if(keyP["b5_nobody_energy_save"] ~= nil) then
		    streams["b5_nobody_energy_save"] = keyP["b5_nobody_energy_save"]
		end
		if(keyP["b5_prevent_straight_wind"] ~= nil) then
		    streams["b5_prevent_straight_wind"] = keyP["b5_prevent_straight_wind"]
		end
		if(keyP["jet_cool"] ~= nil) then
		    streams["jet_cool"] = keyP["jet_cool"]
		end
		if(keyP["b5_jet_cool"] ~= nil) then
		    streams["b5_jet_cool"] = keyP["b5_jet_cool"]
		end
		if(keyP["body_check"] ~= nil) then
		    streams["body_check"] = keyP["body_check"]
		end
		if(keyP["b5_body_check"] ~= nil) then
		    streams["b5_body_check"] = keyP["b5_body_check"]
		end
		if(keyP["b5_rate_select"] ~= nil) then
		    streams["b5_rate_select"] = keyP["b5_rate_select"]
		end
		if(keyP["b5_fresh_air"] ~= nil) then
		    streams["b5_fresh_air"] = keyP["b5_fresh_air"]
		end
		if(keyP["b5_wind_swing_lr_angle"] ~= nil) then
		    streams["b5_wind_swing_lr_angle"] = keyP["b5_wind_swing_lr_angle"]
		end
		if(keyP["b5_wind_swing_ud_angle"] ~= nil) then
		    streams["b5_wind_swing_ud_angle"] = keyP["b5_wind_swing_ud_angle"]
		end
		if(keyP["b5_main_horizontal_guide_strip_1"] ~= nil) then
		    streams["b5_main_horizontal_guide_strip_1"] = keyP["b5_main_horizontal_guide_strip_1"]
		end
		if(keyP["b5_main_horizontal_guide_strip_2"] ~= nil) then
		    streams["b5_main_horizontal_guide_strip_2"] = keyP["b5_main_horizontal_guide_strip_2"]
		end
		if(keyP["b5_main_horizontal_guide_strip_3"] ~= nil) then
		    streams["b5_main_horizontal_guide_strip_3"] = keyP["b5_main_horizontal_guide_strip_3"]
		end
		if(keyP["b5_main_horizontal_guide_strip_4"] ~= nil) then
		    streams["b5_main_horizontal_guide_strip_4"] = keyP["b5_main_horizontal_guide_strip_4"]
		end
		if(keyP["b5_sup_horizontal_guide_strip_3"] ~= nil) then
		    streams["b5_sup_horizontal_guide_strip_3"] = keyP["b5_sup_horizontal_guide_strip_3"]
		end
		if(keyP["b5_sup_horizontal_guide_strip_4"] ~= nil) then
		    streams["b5_sup_horizontal_guide_strip_4"] = keyP["b5_sup_horizontal_guide_strip_4"]
		end
		if(keyP["b5_sup_horizontal_guide_strip_2"] ~= nil) then
		    streams["b5_sup_horizontal_guide_strip_2"] = keyP["b5_sup_horizontal_guide_strip_2"]
		end
		if(keyP["b5_sup_horizontal_guide_strip_1"] ~= nil) then
		    streams["b5_sup_horizontal_guide_strip_1"] = keyP["b5_sup_horizontal_guide_strip_1"]
		end
		if(keyP["b5_twins_machine"] ~= nil) then
		    streams["b5_twins_machine"] = keyP["b5_twins_machine"]
		end
		if(keyP["b5_guide_strip_type"] ~= nil) then
		    streams["b5_guide_strip_type"] = keyP["b5_guide_strip_type"]
		end
		if(keyP["sound"] ~= nil) then
		    streams["sound"] = keyP["sound"]
		end
		if(keyP["b5_sound"] ~= nil) then
		    streams["b5_sound"] = keyP["b5_sound"]
		end
		if(keyP["anion"] ~= nil) then
		    streams["anion"] = keyP["anion"]
		end
		if(keyP["b5_anion"] ~= nil) then
		    streams["b5_anion"] = keyP["b5_anion"]
		end
		if(keyP["b5_parent_control"] ~= nil) then
		    streams["b5_parent_control"] = keyP["b5_parent_control"]
		end
		if(keyP["b5_ieco_switch"] ~= nil) then
		    streams["b5_ieco_switch"] = keyP["b5_ieco_switch"]
		end
		if(keyP["ieco_switch"] ~= nil) then
		    streams["ieco_switch"] = keyP["ieco_switch"]
		end
		if(keyP["ieco_target_rate"] ~= nil) then
		    streams["ieco_target_rate"] = keyP["ieco_target_rate"]
		end
		if(keyP["ieco_indoor_wind_speed"] ~= nil) then
		    streams["ieco_indoor_wind_speed"] = keyP["ieco_indoor_wind_speed"]
		end
		if(keyP["ieco_outdoor_wind_speed"] ~= nil) then
		    streams["ieco_outdoor_wind_speed"] = keyP["ieco_outdoor_wind_speed"]
		end
		if(keyP["ieco_expansion_valve"] ~= nil) then
		    streams["ieco_expansion_valve"] = keyP["ieco_expansion_valve"]
		end
		if(keyP["ieco_frame"] ~= nil) then
		    streams["ieco_frame"] = keyP["ieco_frame"]
		end
		if(keyP["ieco_number"] ~= nil) then
		    streams["ieco_number"] = keyP["ieco_number"]
		end
		if(keyP["wind_around"] ~= nil) then
		    streams["wind_around"] = keyP["wind_around"]
		end
		if(keyP["wind_around_ud"] ~= nil) then
		    streams["wind_around_ud"] = keyP["wind_around_ud"]
		end
		if(keyP["b5_wind_around"] ~= nil) then
		    streams["b5_wind_around"] = keyP["b5_wind_around"]
		end
		if(keyP["prevent_straight_wind_select"] ~= nil) then
		    streams["prevent_straight_wind_select"] = keyP["prevent_straight_wind_select"]
		end
		if(keyP["b5_prevent_straight_wind_select"] ~= nil) then
		    streams["b5_prevent_straight_wind_select"] = keyP["b5_prevent_straight_wind_select"]
		end
		if(keyP["mito_cool"] ~= nil) then
		    streams["mito_cool"] = keyP["mito_cool"]
		end
		if(keyP["mito_heat"] ~= nil) then
		    streams["mito_heat"] = keyP["mito_heat"]
		end
		if(keyP["dr_time"] ~= nil) then
		    streams["dr_time"] = keyP["dr_time"]
		end
		if(keyP["dr_time_hour"] ~= nil) then
		    streams["dr_time_hour"] = keyP["dr_time_hour"]
		end
		if(keyP["dr_time_min"] ~= nil) then
		    streams["dr_time_min"] = keyP["dr_time_min"]
		end
		if(keyP["has_cool_heat_amount"] ~= nil) then
		    streams["has_cool_heat_amount"] = keyP["has_cool_heat_amount"]
		end
		if(keyP["t2_heat"] ~= nil) then
		    streams["t2_heat"] = keyP["t2_heat"]
		end
		if(keyP["tp_heat"] ~= nil) then
		    streams["tp_heat"] = keyP["tp_heat"]
		end
		if(keyP["k1_value"] ~= nil) then
		    streams["k1_value"] = keyP["k1_value"]
		end
		if(keyP["k2_value"] ~= nil) then
		    streams["k2_value"] = keyP["k2_value"]
		end
		if(keyP["k3_value"] ~= nil) then
		    streams["k3_value"] = keyP["k3_value"]
		end
		if(keyP["k4_value"] ~= nil) then
		    streams["k4_value"] = keyP["k4_value"]
		end
		if(keyP["cool_strong_wind_speed"] ~= nil) then
		    streams["cool_strong_wind_speed"] = keyP["cool_strong_wind_speed"]
		end
		if(keyP["cool_strong_wind_amount"] ~= nil) then
		    streams["cool_strong_wind_amount"] = keyP["cool_strong_wind_amount"]
		end
		if(keyP["has_icheck"] ~= nil) then
		    streams["has_icheck"] = keyP["has_icheck"]
		end
		if(keyP["b5_has_icheck"] ~= nil) then
		    streams["b5_has_icheck"] = keyP["b5_has_icheck"]
		end
		if(keyP["b5_emergent_heat_wind"] ~= nil) then
		    streams["b5_emergent_heat_wind"] = keyP["b5_emergent_heat_wind"]
		end
		if(keyP["b5_heat_ptc_wind"] ~= nil) then
		    streams["b5_heat_ptc_wind"] = keyP["b5_heat_ptc_wind"]
		end
		if(keyP["cvp"] ~= nil) then
		    streams["cvp"] = keyP["cvp"]
		end
		if(keyP["b5_cvp"] ~= nil) then
		    streams["b5_cvp"] = keyP["b5_cvp"]
		end
		if(keyP["new_wind_sense"] ~= nil) then
		    streams["new_wind_sense"] = keyP["new_wind_sense"]
		end
		if(keyP["b5_new_wind_sense"] ~= nil) then
		    streams["b5_new_wind_sense"] = keyP["b5_new_wind_sense"]
		end
		if(keyP["in_code"] ~= nil) then
		    streams["in_code"] = keyP["in_code"]
		end
		if(keyP["in_version"] ~= nil) then
		    streams["in_version"] = keyP["in_version"]
		end
		if(keyP["out_code"] ~= nil) then
		    streams["out_code"] = keyP["out_code"]
		end
		if(keyP["out_version"] ~= nil) then
		    streams["out_version"] = keyP["out_version"]
		end
		if(keyP["comfort"] ~= nil) then
		    streams["comfort"] = keyP["comfort"]
		end
		if(keyP["b5_comfort"] ~= nil) then
		    streams["b5_comfort"] = keyP["b5_comfort"]
		end
		if(keyP["b5_air_ieco"] ~= nil) then
		    streams["b5_air_ieco"] = keyP["b5_air_ieco"]
		end
		if(keyP["b5_end_ieco"] ~= nil) then
		    streams["b5_end_ieco"] = keyP["b5_end_ieco"]
		end
	end
	keyP["propertyNumber"] = 0
	keyP["prevent_super_cool"] = nil 
	keyP["prevent_straight_wind"] = nil 
	keyP["auto_prevent_straight_wind"] = nil 
	keyP["prevent_straight_wind_flag"] = nil
	keyP["wind_straight"] = nil 
	keyP["wind_avoid"] = nil 
	keyP["yb_wind_avoid"] = nil
	keyP["intelligent_wind"] = nil 
	keyP["self_clean"] = nil 
	keyP["no_wind_sense"] = nil 
	keyP["no_wind_sense_level"] = nil
	keyP["fn_no_wind_sense"] = nil
	keyP["child_prevent_cold_wind"] = nil 
	keyP["little_angel"] = nil 
	keyP["cool_hot_sense"] = nil 
	keyP["gentle_wind_sense"] = nil 
	keyP["prevent_straight_wind_fa"] = nil 
	keyP["no_wind_sense_fa"] = nil 
	keyP["security"] = nil 
	keyP["even_wind"] = nil
	keyP["single_tuyere"] = nil
	keyP["extreme_wind"] = nil
	keyP["extreme_wind_level"] = nil
	keyP["voice_control"] = nil
	keyP["pre_cool_hot"] = nil
	keyP["water_washing"] = nil
	keyP["fresh_air"] = nil
	keyP["fa_prevent_straight_wind"] = nil
	keyP["parent_control"] = nil
	keyP["parent_control_temp_up"] = nil
	keyP["parent_control_temp_down"] = nil
	keyP["nobody_energy_save"] = nil
	keyP["filter_value"] = nil
	keyP["filter_level"] = nil
	keyP["prevent_straight_wind_lr"] = nil
	keyP["pm25_value"] = nil
	keyP["water_pump"] = nil
	keyP["intelligent_control"] = nil
	keyP["volume_control"] = nil
	keyP["voice_control_new"] = nil
	keyP["wind_swing_ud_angle"] = nil
	keyP["wind_swing_lr_angle"] = nil
	keyP["face_register"] = nil
	keyP["cool_temp_up"] = nil
	keyP["cool_temp_down"] = nil
	keyP["auto_temp_up"] = nil
	keyP["auto_temp_down"] = nil
	keyP["heat_temp_up"] = nil
	keyP["heat_temp_down"] = nil
	keyP["remote_control_lock"] = nil
	keyP["remote_control_lock_control"] = nil
	keyP["operating_time"] = nil
	keyP["indoor_humidity"] = nil
	keyP["rate_select"] = nil
	keyP["b5_mode"] = nil
	keyP["b5_strong_wind"] = nil
	keyP["b5_wind_speed"] = nil
	keyP["b5_humidity"] = nil
	keyP["b5_temperature_0"] = nil
	keyP["b5_temperature_1"] = nil
	keyP["b5_temperature_2"] = nil
	keyP["b5_temperature_3"] = nil
	keyP["b5_temperature_4"] = nil
	keyP["b5_temperature_5"] = nil
	keyP["b5_temperature_6"] = nil
	keyP["b5_eco"] = nil
	keyP["b5_filter_remind"] = nil
	keyP["b5_filter_check"] = nil
	keyP["b5_fahrenheit"] = nil
	keyP["b5_8_heat"] = nil
	keyP["b5_electricity"] = nil
	keyP["b5_ptc"] = nil
	keyP["b5_wind_straight"] = nil
	keyP["b5_wind_avoid"] = nil
	keyP["b5_wind_swing"] = nil
	keyP["b5_no_wind_sense"] = nil
	keyP["b5_screen_display"] = nil
	keyP["b5_anion"] = nil
	keyP["b5_self_clean"] = nil
	keyP["b5_fa_no_wind_sense"] = nil
	keyP["b5_nobody_energy_save"] = nil
	keyP["b5_prevent_straight_wind"] = nil
	keyP["real_time_power"] = nil
	keyP["real_time_power_10"] = nil
	keyP["current_humidity"] = nil
	keyP["jet_cool"] = nil
	keyP["b5_jet_cool"] = nil
	keyP["body_check"] = nil
	keyP["b5_body_check"] = nil
	keyP["b5_rate_select"] = nil
	keyP["b5_fresh_air"] = nil
	keyP["b5_wind_swing_lr_angle"] = nil
	keyP["b5_wind_swing_ud_angle"] = nil
	keyP["main_horizontal_guide_strip_1"] = nil
	keyP["main_horizontal_guide_strip_2"] = nil
	keyP["main_horizontal_guide_strip_3"] = nil
	keyP["main_horizontal_guide_strip_4"] = nil
	keyP["sup_horizontal_guide_strip_1"] = nil
	keyP["sup_horizontal_guide_strip_2"] = nil
	keyP["sup_horizontal_guide_strip_3"] = nil
	keyP["sup_horizontal_guide_strip_4"] = nil
	keyP["twins_machine"] = nil
	keyP["guide_strip_type"] = nil
	keyP["b5_main_horizontal_guide_strip_1"] = nil
	keyP["b5_main_horizontal_guide_strip_2"] = nil
	keyP["b5_main_horizontal_guide_strip_3"] = nil
	keyP["b5_main_horizontal_guide_strip_4"] = nil
	keyP["b5_sup_horizontal_guide_strip_1"] = nil
	keyP["b5_sup_horizontal_guide_strip_2"] = nil
	keyP["b5_sup_horizontal_guide_strip_3"] = nil
	keyP["b5_sup_horizontal_guide_strip_4"] = nil
	keyP["b5_twins_machine"] = nil
	keyP["b5_guide_strip_type"] = nil
	keyP["b5_sound"] = nil
	keyP["sound"] = nil
	keyP["b5_anion"] = nil
	keyP["anion"] = nil
	keyP["machine_type"] = nil
	keyP["product_type"] = nil
	keyP["independent_ptc"] = nil
	keyP["fa_no_wind_sense"] = nil
	keyP["b5_parent_control"] = nil
	keyP["ieco_switch"] = nil
	keyP["ieco_target_rate"] = nil
	keyP["ieco_indoor_wind_speed"] = nil
	keyP["ieco_outdoor_wind_speed"] = nil
	keyP["ieco_expansion_valve"] = nil
	keyP["ieco_frame"] = nil
	keyP["ieco_number"] = nil
	keyP["b5_ieco_switch"] = nil
	keyP["wind_around"] = nil
	keyP["wind_around_ud"] = nil
	keyP["b5_wind_around"] = nil
	keyP["prevent_straight_wind_select"] = nil
	keyP["b5_prevent_straight_wind_select"] = nil
	keyP["mito_cool"] = nil
	keyP["mito_heat"] = nil
	keyP["dr_time"] = nil
	keyP["dr_time_hour"] = nil
	keyP["dr_time_min"] = nil
    keyP["t2_heat"] = nil
	keyP["tp_heat"] = nil
	keyP["k1_value"] = nil
	keyP["k2_value"] = nil
	keyP["k3_value"] = nil
	keyP["k4_value"] = nil
	keyP["cool_strong_wind_speed"] = nil
	keyP["cool_strong_wind_amount"] = nil
	keyP["has_cool_heat_amount"] = nil
	keyP["has_icheck"] = nil
	keyP["b5_has_icheck"] = nil
	keyP["b5_emergent_heat_wind"] = nil
	keyP["b5_heat_ptc_wind"] = nil
	keyP["cvp"] = nil
	keyP["b5_cvp"] = nil
	keyP["new_wind_sense"] = nil
	keyP["b5_new_wind_sense"] = nil
	keyP["in_code"] = nil
	keyP["in_version"] = nil
	keyP["out_code"] = nil
	keyP["out_version"] = nil
	keyP["comfort"] = nil
	keyP["b5_comfort"] = nil
	keyP["b5_air_ieco"] = nil
	keyP["b5_end_ieco"] = nil
	local retTable = {}
    retTable["status"] = streams
    local ret = encode(retTable)
    return ret
end


