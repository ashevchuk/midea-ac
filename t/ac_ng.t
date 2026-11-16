#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../lua";

# ac-ng.pl is a script, not a module, but its CLI/Getopt/network driver is
# guarded by `unless (caller)` precisely so this test can `require` it and
# call its internal subs (lua_run, lua_table_literal, json_decode, ...)
# directly against the real device .lua, no hardware involved. This is a
# regression guard for ac-ng.pl's own glue code (the driver snippets
# cmd_get/cmd_set build) — cross_check_luajit.t already covers the
# PureLua engine itself and is not duplicated here.
require "$Bin/../ac-ng.pl";

my $device_script = "$Bin/../lua/T_0000_AC_00000Q1B_2024013001.lua";
plan skip_all => "device script not present: $device_script" unless -f $device_script;

open my $fh, '<', $device_script or die $!;
local $/;
my $device_src = <$fh>;
close $fh;

# --- lua_literal / lua_table_literal: pure Perl, no engine needed ---

is( lua_literal(24),    '24',   'lua_literal: integer passes through bare' );
is( lua_literal('0.5'), '0.5',  'lua_literal: decimal passes through bare' );
is( lua_literal('on'),  '"on"', 'lua_literal: bare word gets quoted' );

is(
    lua_table_literal( mode => 'cool', temperature => 24 ),
    '{mode = "cool", temperature = 24}',
    'lua_table_literal: sorted keys, mixed string/number values'
);

{
    # Round-trip a value with both a quote and a backslash through the
    # real engine, rather than hand-computing the expected escape
    # sequence — a byte-for-byte run is the only check that actually
    # catches an escaping bug (same rationale as cross_check_luajit.t).
    my $raw = 'a' . '"' . 'b' . '\\' . 'c';
    my $back = lua_run( '', 'return ' . lua_literal($raw) );
    is( $back, $raw, 'lua_literal: round-trips a quote+backslash through the real engine' );
}

# --- lua_run driving the real device script's own jsonToData/dataToJson,
# exactly as cmd_set/cmd_get build it ---

my $device_info = DEVICE_INFO_LUA();

my $control = lua_table_literal(
    power => 'on', mode => 'cool', temperature => 24, small_temperature => 0,
    wind_speed => 50, wind_swing_ud => 'off', wind_swing_lr => 'off',
    strong_wind => 'off', comfort_sleep => 'off', purifier => 'off',
    eco => 'off', dry => 'off', prevent_cold => 'off', power_saving => 'off',
    ptc => 'off', sleep_status => 0,
);

my $hex = lua_run(
    $device_src,
    qq{local JSON = require "cjson"\n}
  . qq{local env = { deviceinfo = $device_info, control = $control }\n}
  . qq{return jsonToData(JSON.encode(env))}
);

like( $hex, qr/^[0-9a-f]+$/, 'lua_run(jsonToData): returns a hex string' );
is( length($hex) % 2, 0, 'lua_run(jsonToData): even number of hex digits' );

# Byte 10 (0-indexed; hex chars 21-22) is the frame-type selector: 0x40 for
# a baseline control with no extended ("B0") property requested.
is( substr( $hex, 20, 2 ), '40', 'lua_run(jsonToData): baseline control yields a regular (0x40) frame' );

# Swap the type byte to 0xc0 (status) and feed it back through dataToJson —
# this is cmd_get's own code path (ahex()+dataToJson+json_decode), not
# just jsonToData in isolation.
my $status_hex = substr( $hex, 0, 20 ) . 'c0' . substr( $hex, 22 );
my $json_text = lua_run(
    $device_src,
    qq{local JSON = require "cjson"\n}
  . qq{local env = { deviceinfo = $device_info, msg = { data = "$status_hex" } }\n}
  . qq{return dataToJson(JSON.encode(env))}
);

my $decoded = json_decode($json_text);
is( $decoded->{status}{power},       'on',   'lua_run(dataToJson): power round-trips' );
is( $decoded->{status}{mode},        'cool', 'lua_run(dataToJson): mode round-trips' );
is( $decoded->{status}{temperature}, 24,     'lua_run(dataToJson): temperature round-trips' );

# --- a control variant that takes the B0 extended-property path ---
# ("security" is one of several fields that increment the real script's
# own keyP.propertyNumber — see PROTOCOL_NOTES.md / the pure-lua-engine
# memory). Structural check only: ac-ng.pl's cmd_set never special-cases
# this, it's entirely the device script's own job to pick 0x40 vs 0xb0.

my $control_b0 = lua_table_literal(
    power => 'on', mode => 'cool', temperature => 24, small_temperature => 0,
    wind_speed => 50, wind_swing_ud => 'off', wind_swing_lr => 'off',
    strong_wind => 'off', comfort_sleep => 'off', purifier => 'off',
    eco => 'off', dry => 'off', prevent_cold => 'off', power_saving => 'off',
    ptc => 'off', sleep_status => 0, security => 'on',
);

my $hex_b0 = lua_run(
    $device_src,
    qq{local JSON = require "cjson"\n}
  . qq{local env = { deviceinfo = $device_info, control = $control_b0 }\n}
  . qq{return jsonToData(JSON.encode(env))}
);

is( substr( $hex_b0, 20, 2 ), 'b0', 'lua_run(jsonToData): a propertyNumber-incrementing field yields a B0 frame' );

done_testing;
