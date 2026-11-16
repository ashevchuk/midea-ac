#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../lua";

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

# encode: array vs. object detection, nesting, scalar types.
{
    my ($a, $b, $c) = run_lua(<<'LUA');
local JSON = require "cjson"
local arr = JSON.encode({1, 2, 3})
local obj = JSON.encode({a = 1, b = "x"})
local mixed = JSON.encode({1, 2, foo = "bar"})
return arr, obj, mixed
LUA
    is($a, '[1,2,3]', 'a contiguous-from-1 table encodes as a JSON array');
    is($b, '{"a":1,"b":"x"}', 'a table with string keys encodes as a JSON object');
    is($c, '{"1":1,"2":2,"foo":"bar"}', 'a table with both integer and string keys falls back to an object (matches cjson.lua\'s own is_array check)');
}

{
    my ($s) = run_lua('local JSON = require "cjson"; return JSON.encode({nested = {1, 2}, flag = true, empty = {}})');
    is($s, '{"nested":[1,2],"flag":true,"empty":[]}', 'nested tables, booleans, and an empty table (-> [])');
}

{
    my ($s) = run_lua(q{local JSON = require "cjson"; return JSON.encode("a\"b\nc")});
    is($s, '"a\\"b\\nc"', 'string encoding escapes quotes and control characters');
}

# decode: round-trips through the same shapes, and reaches into the
# result exactly the way the real device script does (nested index
# chains like json["deviceinfo"]["deviceSubType"]).
{
    my ($subtype, $sn, $power) = run_lua(<<'LUA');
local JSON = require "cjson"
local json = JSON.decode('{"deviceinfo":{"deviceSubType":2,"deviceSN":"00Q1B"},"control":{"power":"on"}}')
return json["deviceinfo"]["deviceSubType"], json["deviceinfo"]["deviceSN"], json["control"]["power"]
LUA
    is($subtype, 2, 'decoded nested number field');
    is($sn, '00Q1B', 'decoded nested string field');
    is($power, 'on', 'decoded a second top-level nested object');
}

{
    my ($a1, $a2, $a3, $n) = run_lua(<<'LUA');
local JSON = require "cjson"
local t = JSON.decode('[10, 20, 30]')
return t[1], t[2], t[3], #t
LUA
    is($a1, 10, 'decoded array element 1');
    is($a2, 20, 'decoded array element 2');
    is($a3, 30, 'decoded array element 3');
    is($n, 3, '#t on a decoded JSON array matches its length');
}

# A true round trip: encode, decode, re-encode, compare.
{
    my ($rt) = run_lua(<<'LUA');
local JSON = require "cjson"
local original = {power = "on", temperature = 24, fan = {1, 2, 3}}
local encoded = JSON.encode(original)
local decoded = JSON.decode(encoded)
return JSON.encode(decoded)
LUA
    is($rt, '{"power":"on","temperature":24,"fan":[1,2,3]}', 'encode -> decode -> encode round-trips to the same JSON');
}

done_testing;
