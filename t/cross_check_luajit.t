#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../reverse/lua";
use File::Temp qw(tempfile);

use PureLua::Parser;
use PureLua::Eval;
use PureLua::Stdlib;

# Differential check against real luajit, per the design doc's
# verification plan (.notes/2026-11-16-pure-perl-lua-engine-design.md):
# every one of this engine's bugs so far (truncated frames from a
# missing table-key split, a dropped tonumber(s,16) base argument) was
# silent corruption, not a crash - a byte-for-byte diff against the real
# interpreter is the only check that actually catches that class of bug.

my $luajit = `which luajit 2>/dev/null`;
chomp $luajit;
plan skip_all => 'luajit not installed - cannot cross-check against the real interpreter' unless $luajit;

my $device_script = "$Bin/../reverse/lua/T_0000_AC_00000Q1B_2024013001.lua";
plan skip_all => "device script not present: $device_script" unless -f $device_script;

# Make `require "cjson"` resolve to this project's own shim regardless
# of luajit's working directory.
local $ENV{LUA_PATH} = "$Bin/../reverse/lua/?.lua;;";

my $BASE_CONTROL = <<'LUA';
{
  power = "on", mode = "cool", temperature = 24, small_temperature = 0,
  wind_speed = 50, wind_swing_ud = "off", wind_swing_lr = "off",
  strong_wind = "off", comfort_sleep = "off", purifier = "off",
  eco = "off", dry = "off", prevent_cold = "off", power_saving = "off",
  ptc = "off", sleep_status = 0,
}
LUA

my @VARIANT_OVERRIDES = (
    '{}',
    '{eco = "on"}',
    '{mode = "heat", temperature = 18}',
    '{wind_speed = 30, purifier = "on"}',
    '{power = "off"}',
    '{dry = "on", prevent_cold = "on"}',
);

sub run_luajit {
    my ($lua_src) = @_;
    my ($fh, $path) = tempfile(SUFFIX => '.lua');
    print {$fh} $lua_src;
    close $fh;
    my $out = `luajit '$path' 2>&1`;
    unlink $path;
    die "luajit failed: $out\n" if $? != 0;
    return $out;
}

sub build_variants_lua {
    my $n = scalar @VARIANT_OVERRIDES;
    my $lua = qq{local BASE_CONTROL = $BASE_CONTROL\n};
    $lua .= "local function merge(base, extra) local t = {} for k,v in pairs(base) do t[k]=v end for k,v in pairs(extra) do t[k]=v end return t end\n";
    for my $i (0 .. $n - 1) {
        $lua .= "local v$i = merge(BASE_CONTROL, $VARIANT_OVERRIDES[$i])\n";
    }
    return $lua;
}

# --- encode (jsonToData) direction, across several control variants ---

my $luajit_driver = build_variants_lua() . <<LUA;
local JSON = require "cjson"
dofile("$device_script")
local vars = { v0, v1, v2, v3, v4, v5 }
for i = 1, #vars do
    local env = { deviceinfo = { deviceSubType = 2, deviceSN = "00000000000000Q1B0" }, control = vars[i] }
    print(jsonToData(JSON.encode(env)))
end
LUA

my @luajit_frames = split /\n/, run_luajit($luajit_driver);
is(scalar(@luajit_frames), scalar(@VARIANT_OVERRIDES), 'luajit produced one frame per control variant');

my $perl_driver = build_variants_lua() . <<'LUA';
local JSON = require "cjson"
local vars = { v0, v1, v2, v3, v4, v5 }
local results = {}
for i = 1, #vars do
    local env = { deviceinfo = { deviceSubType = 2, deviceSN = "00000000000000Q1B0" }, control = vars[i] }
    results[i] = jsonToData(JSON.encode(env))
end
return results[1], results[2], results[3], results[4], results[5], results[6]
LUA

open my $fh, '<', $device_script or die $!;
local $/;
my $device_src = <$fh>;
close $fh;

my $eval = PureLua::Eval->new;
PureLua::Stdlib->install($eval);
my $ast = PureLua::Parser->parse($device_src . "\n" . $perl_driver);
my @our_frames = map { $eval->str_value($_) } $eval->run($ast);

for my $i (0 .. $#VARIANT_OVERRIDES) {
    my $l = $luajit_frames[$i] // '';
    my $o = $our_frames[$i]   // '';
    my $label = "variant " . ($i + 1) . " (" . ($VARIANT_OVERRIDES[$i] eq '{}' ? 'base' : $VARIANT_OVERRIDES[$i]) . ")";

    is(length($o), length($l), "$label: frame length matches luajit");

    # Exclude the trailing 3 bytes (6 hex chars): a math.random() message-id
    # byte plus its two cascading checksums, seeded from os.time() - these
    # differ between any two calls not made in the same wall-clock second,
    # by design (design doc gotcha #2), not a real discrepancy.
    my $trim = length($l) >= 6 ? length($l) - 6 : 0;
    is(substr($o, 0, $trim), substr($l, 0, $trim), "$label: bytes match luajit, excluding the random/checksum tail");
}

# --- decode (dataToJson) direction: swap the first frame's type byte to
# 0xc0 (status) and diff the fully-decoded field set, order-independent.

my $base_frame = $luajit_frames[0];
my $status_frame = substr($base_frame, 0, 20) . 'c0' . substr($base_frame, 22);

my $luajit_decode_driver = <<LUA;
local JSON = require "cjson"
dofile("$device_script")
local env = { deviceinfo = { deviceSubType = 2, deviceSN = "00000000000000Q1B0" }, msg = { data = "$status_frame" } }
print(dataToJson(JSON.encode(env)))
LUA
my $luajit_decoded_json = run_luajit($luajit_decode_driver);
chomp $luajit_decoded_json;

my $decode_wrapper = <<"LUA";
local JSON = require "cjson"
local env = { deviceinfo = { deviceSubType = 2, deviceSN = "00000000000000Q1B0" }, msg = { data = "$status_frame" } }
return dataToJson(JSON.encode(env))
LUA
my $eval2 = PureLua::Eval->new;
PureLua::Stdlib->install($eval2);
my $ast2 = PureLua::Parser->parse($device_src . "\n" . $decode_wrapper);
my ($our_decoded_json) = map { $eval2->str_value($_) } $eval2->run($ast2);

sub extract_status_fields {
    my ($json_text) = @_;
    my %fields;
    while ($json_text =~ /"([^"]+)":("(?:[^"\\]|\\.)*"|-?[\d.]+|true|false|null)/g) {
        $fields{$1} = $2;
    }
    return \%fields;
}

my $luajit_fields = extract_status_fields($luajit_decoded_json);
my $our_fields    = extract_status_fields($our_decoded_json);

is_deeply($our_fields, $luajit_fields, 'dataToJson: every decoded status field matches luajit exactly');

done_testing;
