#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../reverse/lua";

use PureLua::Parser;
use PureLua::Eval;
use PureLua::Stdlib;

sub run_lua {
    my ($src) = @_;
    my $ast  = PureLua::Parser->parse($src);
    my $eval = PureLua::Eval->new;
    PureLua::Stdlib->install($eval);
    return map { $eval->str_value($_) } $eval->run($ast);
}

# tonumber / tostring.
{
    my ($a, $b, $c) = run_lua('return tonumber("42"), tonumber("nope"), tostring(42)');
    is($a, 42, 'tonumber coerces a numeric string');
    is($b, undef, 'tonumber returns nil for a non-numeric string');
    is($c, '42', 'tostring formats a number as a plain string');
}

# tonumber(s, base): the real device script's string2table helper does
# tonumber(doublebytestr, 16) to parse hex byte pairs - a plain decimal
# parse happened to be right for digit-only pairs but silently nil'd
# out actual hex letters, deleting that table slot. Caught by
# cross-checking the real script's dataToJson against luajit.
{
    my ($hex_aa, $hex_02, $bad) = run_lua('return tonumber("aa", 16), tonumber("02", 16), tonumber("zz", 16)');
    is($hex_aa, 170, 'tonumber with an explicit base parses hex letters');
    is($hex_02, 2, 'tonumber with an explicit base still parses a digit-only pair correctly');
    is($bad, undef, 'tonumber with an explicit base returns nil for a digit out of that base');
}

# pairs, over a plain map table (insertion order, since that's our
# documented deterministic choice).
{
    my ($count, $sum) = run_lua(<<'LUA');
local t = {a = 1, b = 2, c = 3}
local n, s = 0, 0
for k, v in pairs(t) do
    n = n + 1
    s = s + v
end
return n, s
LUA
    is($count, 3, 'pairs visits every entry');
    is($sum, 6, 'pairs yields the right values');
}

# math.floor / math.modf.
{
    my ($f1, $f2, $ip, $fp) = run_lua('return math.floor(3.7), math.floor(-3.7), math.modf(5.25)');
    is($f1, 3, 'math.floor rounds down for a positive number');
    is($f2, -4, 'math.floor rounds down (toward -inf) for a negative number');
    is($ip, 5, 'math.modf integer part truncates toward zero');
    is($fp, 0.25, 'math.modf fractional part');
}

# math.random / math.randomseed: not exact values, just the documented
# contract (range, and same-seed determinism).
{
    my ($same1, $same2, $in_range) = run_lua(<<'LUA');
math.randomseed(1)
local a = math.random(100)
math.randomseed(1)
local b = math.random(100)
local c = math.random(5, 5)
return a, b, c
LUA
    is($same1, $same2, 'same seed produces the same math.random() draw');
    is($in_range, 5, 'math.random(m, m) always returns m');
}

# os.time: just that it returns a plausible current epoch value.
{
    my ($t) = run_lua('return os.time()');
    ok($t > 1_700_000_000, 'os.time() returns a real-looking epoch second');
}

# bit library: band/bor/bxor/bnot/lshift/rshift, against hand-checked values.
{
    my @r = run_lua(<<'LUA');
return bit.band(0xF0, 0x0F), bit.bor(0xF0, 0x0F), bit.bxor(0xFF, 0x0F),
       bit.lshift(1, 4), bit.rshift(0x100, 4)
LUA
    is_deeply(\@r, [ 0x00, 0xFF, 0xF0, 0x10, 0x10 ], 'bit.band/bor/bxor/lshift/rshift match hand-checked values');
}

# string.char/sub/rep/format.
{
    my @r = run_lua(<<'LUA');
return string.char(65, 66, 67),
       string.sub("hello world", 1, 5),
       string.sub("hello world", -5),
       string.rep("ab", 3),
       string.format("%02x", 255),
       string.format("%d-%s", 7, "x")
LUA
    is_deeply(
        \@r,
        [ 'ABC', 'hello', 'world', 'ababab', 'ff', '7-x' ],
        'string.char/sub (incl. negative index)/rep/format'
    );
}

# string.find/match/gmatch/gsub - the pattern-matching surface, run
# through the full pipeline this time (Pattern.pm already has its own
# focused tests in t/pattern.t for the matching logic itself).
{
    my @r = run_lua(<<'LUA');
local a, b = string.find("key=value", "=")
local m = string.match("count: 42", "%d+")
local out, n = string.gsub("a,b,c", ",", ";")
return a, b, m, out, n
LUA
    is_deeply(\@r, [ 4, 4, '42', 'a;b;c', 2 ], 'string.find/match/gsub through the full Lua pipeline');

    # string.gmatch as a generic-for iterator - this is the actual real-
    # script usage pattern (splitStrByChar), not just a direct call.
    my ($joined) = run_lua(<<'LUA');
local parts = {}
for s in string.gmatch("aa,bb,cc", '[^,]+') do
    table.insert(parts, s)
end
return parts[1] .. "|" .. parts[2] .. "|" .. parts[3]
LUA
    is($joined, 'aa|bb|cc', 'string.gmatch drives a generic for-loop, mirroring splitStrByChar');
}

# table.insert: append form and positional-insert form.
{
    my ($a, $b, $c) = run_lua(<<'LUA');
local t = {10, 20, 30}
table.insert(t, 40)
table.insert(t, 1, 5)
return t[1], t[2], t[5]
LUA
    is($a, 5, 'table.insert(t, pos, v) shifts existing elements up');
    is($b, 10, 'the element that was at position 1 is now at position 2');
    is($c, 40, 'table.insert(t, v) appended at the end');
}

# string.reverse / string.byte, and method-call syntax on a plain
# string (`s:method(...)`) - the real device script turned out to use
# this despite an earlier note that it didn't (str:byte(i),
# tostring(x):reverse():sub(1,7)).
{
    my @r = run_lua(<<'LUA');
local s = "abc"
return s:reverse(), s:byte(2), string.byte("Z"), type(42), type("x"), type({}), type(nil), type(print)
LUA
    is_deeply(
        \@r,
        [ 'cba', 98, 90, 'number', 'string', 'table', 'nil', 'function' ],
        'string method-call dispatch (:reverse/:byte), string.byte default index, and type()'
    );
}

# The real bug this uncovered while loading the actual device script:
# method-call dispatch must not route through _typename's number/string
# heuristic, because a numeric-looking string (tostring(123) -> "123")
# gets classified 'number' there and would otherwise be refused a
# string-library method call it's legitimately entitled to.
{
    my ($r) = run_lua('return tostring(123):reverse():sub(1, 2)');
    is($r, '32', 'method-call dispatch on a numeric-looking string still routes through the string library');
}

# Table-shaped #t border check, matching the exact gotcha the design
# doc flagged (empirically confirmed against real luajit first - see
# .notes/2026-11-16-pure-lua-engine-progress.md): a 0-indexed table with
# no gaps reports the real Lua answer (4, not 5 - index 0 doesn't count).
{
    my ($n) = run_lua(<<'LUA');
local t = {}
for i = 0, 4 do t[i] = i * 10 end
return #t
LUA
    is($n, 4, '#t on a 0-indexed contiguous table matches real luajit (4, not 5)');
}

# pcall and require("bit") - both found missing while sweeping the
# wider 0xAC corpus: pcall is a plain Lua builtin any script might use,
# and several real device scripts do `local bit = require "bit"`
# instead of relying on the pre-installed global (LuaJIT exposes bit
# both ways). The require("bit") case was initially invisible to a
# naive `grep -n` against these files - several are ISO-8859, not
# UTF-8, and plain grep silently treats them as binary and matches
# nothing; `grep -a` (or checking file encoding first) is needed.
{
    my ($ok1, $v1, $ok2, $err) = run_lua(<<'LUA');
local function f(x) return x * 2 end
local function g() local n = nil return n + 1 end
local ok1, v1 = pcall(f, 21)
local ok2, err = pcall(g)
return ok1, v1, ok2, err
LUA
    ok(PureLua::Eval::_is_truthy($ok1), 'pcall reports success for a function that returns normally');
    is($v1, 42, 'pcall forwards the successful call\'s return value');
    ok(!PureLua::Eval::_is_truthy($ok2), 'pcall reports failure for a function that errors');
    like($err, qr/arithmetic/, 'pcall returns the error message on failure');
}

{
    my ($band_result) = run_lua('local bit = require "bit"; return bit.band(0xF0, 0x0F)');
    is($band_result, 0, 'require("bit") returns a working bit library, same as the global');
}

# math.fmod: 94 corpus files use it (checksum helpers mostly). C fmod
# semantics (remainder keeps the dividend's sign), distinct from Lua's
# own `%` which is floor-based - found missing while sweeping the wider
# corpus beyond the one model already cross-checked against luajit.
{
    my ($a, $b) = run_lua('return math.fmod(13, 4), math.fmod(-13, 4)');
    is($a, 1, 'math.fmod matches integer % for positive operands');
    is($b, -1, 'math.fmod keeps the dividend\'s sign (C semantics, not floor-based %)');
}

# bit.* must fail loudly on a non-number argument, matching real
# LuaJIT's own strictness - found via the differential corpus sweep,
# where our silent "treat nil as 0" was quietly diverging from luajit's
# own error instead of failing the same way.
{
    eval { run_lua('return bit.bor(5, nil)') };
    like($@, qr/number expected/, 'bit.bor errors on a nil argument instead of silently treating it as 0');
}

# A Lua STRING holding hex notation ("0x2010") must coerce correctly in
# arithmetic/bit contexts. Perl's own "+0"/int() silently give 0 for
# such a string (they don't understand a "0x" prefix) - found via the
# wider corpus sweep hitting a real `local u16_table = "0x2010"`.
{
    my ($n, $band) = run_lua('return "0x2010" + 0, bit.band("0x2010", 0xffff)');
    is($n, 0x2010, 'a hex-notation Lua string coerces correctly in arithmetic');
    is($band, 0x2010, 'a hex-notation Lua string coerces correctly as a bit.* argument');
}

done_testing;
