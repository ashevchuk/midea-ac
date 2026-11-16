#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../reverse/lua";

use PureLua::Pattern;

# 1. literal substring.
{
    my @r = PureLua::Pattern::find('hello,world', ',');
    is_deeply(\@r, [6, 6], 'literal "," found at its real position');
    my $m = PureLua::Pattern::match('hello,world', ',');
    is($m, ',', 'match() returns the literal match itself');
}

# 2. %d+ digit runs, via gmatch - the exact shape used by the real
# scripts' message-field parsing.
{
    my $it = PureLua::Pattern::gmatch_iterator('a12 b345 c6', '%d+');
    my @all;
    while (defined(my $m = $it->())) { push @all, $m; }
    is_deeply(\@all, [ '12', '345', '6' ], 'gmatch collects every %d+ run');
}

# 3. the real splitStrByChar pattern: '[^' .. sepChar .. ']+', built
# dynamically from a one-character separator.
{
    my $sepChar = ',';
    my $pattern = '[^' . $sepChar . ']+';
    my $it = PureLua::Pattern::gmatch_iterator('aa,bb,cc', $pattern);
    my @parts;
    while (defined(my $m = $it->())) { push @parts, $m; }
    is_deeply(\@parts, [ 'aa', 'bb', 'cc' ], 'negated single-char class splits on the separator');
}

# 4. a-z style range inside a set (cheap, standard character-class
# parsing - not itself seen in the corpus, but free once sets exist).
{
    my @r = PureLua::Pattern::find('123abc456', '[a-z]+');
    is_deeply(\@r, [4, 6], 'a-z range inside a set matches the letter run');
}

# 5. gsub with a literal string replacement and with a function
# replacement (the two kinds the real scripts' gsub call-sites use).
{
    my ($out, $n) = PureLua::Pattern::gsub('a,b,c', ',', ';');
    is($out, 'a;b;c', 'gsub with a literal string replacement');
    is($n, 2, 'gsub reports the substitution count');

    my ($out2, $n2) = PureLua::Pattern::gsub('a1b22c3', '%d+', sub { return '[' . $_[0] . ']'; });
    is($out2, 'a[1]b[22]c[3]', 'gsub with a function replacement');
    is($n2, 3, 'gsub with a function replacement reports the right count');

    my ($out3, undef) = PureLua::Pattern::gsub('abc', '%d+', sub { return undef; });
    is($out3, 'abc', 'a function replacement returning undef keeps the original (no match here, trivially true)');
}

# 6. unsupported constructs fail loudly rather than silently mismatching.
{
    eval { PureLua::Pattern::find('abc', '^abc') };
    like($@, qr/unsupported pattern construct/, 'anchor ^ is explicitly rejected, not silently ignored');

    eval { PureLua::Pattern::find('abc', '%a+') };
    like($@, qr/unsupported pattern class/, '%a (not in the documented vocabulary) is explicitly rejected');
}

done_testing;
