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
