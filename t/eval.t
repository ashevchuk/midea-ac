#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../reverse/lua";

use PureLua::Parser;
use PureLua::Eval;

sub run_lua {
    my ($src) = @_;
    my $ast  = PureLua::Parser->parse($src);
    my $eval = PureLua::Eval->new;
    return map { $eval->str_value($_) } $eval->run($ast);
}

# A. arithmetic + table + numeric for-loop.
{
    my ($sum, $t3) = run_lua(<<'LUA');
local t = {}
local sum = 0
for i = 1, 5 do
    t[i] = i * i
    sum = sum + t[i]
end
return sum, t[3]
LUA
    is($sum, 55, 'sum of squares 1..5 via a numeric for-loop and a table');
    is($t3, 9, 'table index written inside the loop is readable after it');
}

# B. closures sharing an upvalue cell, and independent instances not
# sharing one - the two halves of the "risky part" the design doc flags.
{
    my ($a, $b) = run_lua(<<'LUA');
local function makeCounter()
    local n = 0
    local function inc()
        n = n + 1
        return n
    end
    return inc
end
local c1 = makeCounter()
local c2 = makeCounter()
c1()
c1()
local a = c1()
local b = c2()
return a, b
LUA
    is($a, 3, 'c1 shares one upvalue cell across its own three calls');
    is($b, 1, 'c2 has its own independent cell, unaffected by c1');
}

# C. the classic Lua-5.1 gotcha: each for-loop iteration gets a FRESH
# local, so closures built in different iterations must not alias.
{
    my ($x, $y, $z) = run_lua(<<'LUA');
local fns = {}
for i = 1, 3 do
    local function f() return i end
    fns[i] = f
end
return fns[1](), fns[2](), fns[3]()
LUA
    is($x, 1, 'closure from iteration 1 kept its own i');
    is($y, 2, 'closure from iteration 2 kept its own i (not aliased to the others)');
    is($z, 3, 'closure from iteration 3 kept its own i');
}

# D. if/elseif/else, comparisons, string concatenation, function calls.
{
    my ($result) = run_lua(<<'LUA');
local function classify(n)
    if n < 0 then
        return "neg"
    elseif n == 0 then
        return "zero"
    else
        return "pos"
    end
end
return classify(-5) .. "," .. classify(0) .. "," .. classify(5)
LUA
    is($result, 'neg,zero,pos', 'if/elseif/else branches, comparisons, and .. concatenation');
}

# E. multi-value truncation: only the LAST expression in a list expands.
{
    my @vals = run_lua(<<'LUA');
local function pair() return 1, 2 end
local a, b = pair()
local c, d, e = pair(), 3
return a, b, c, d, e
LUA
    is_deeply(
        \@vals,
        [ 1, 2, 1, 3, undef ],
        'pair() expands fully only when last; truncates to 1 value otherwise, missing values are nil'
    );
}

done_testing;
