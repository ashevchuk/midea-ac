#!/usr/bin/env perl
#
# lua2map.pl — one-time, offline generator: given a Midea-cloud-issued
# device-specific protocol lua script (from `ac-check.pl --lua`), derives
# a byte/bit-level protocol map by probing the script's own jsonToData/
# dataToJson entry points through luajit, and writes <model>.protocol.json
# next to it.
#
# This is a dev-time tool only. It is the one place in this project that
# depends on luajit; the runtime tool that consumes its output (ac-ng.pl)
# does not.
#
use strict;
use warnings;

my $lua_file = shift @ARGV
  or die "usage: lua2map.pl <device.lua>\n";
die "not found: $lua_file\n" unless -f $lua_file;

my ($dir, $base) = ( $lua_file =~ m{^(.*/)?([^/]+)$} );
$dir //= './';
my ($model) = ( $base =~ /^T_0000_AC_([0-9A-Za-z]+)_\d+\.lua$/ );
$model //= $base;
$model =~ s/\.lua$//;

my $harness = "$dir/harness.lua";
$harness = './harness.lua' unless -f $harness;
die "harness.lua not found next to lua2map.pl or the device file\n"
  unless -f $harness;

my $out_file = "$dir$model.protocol.json";

print "Probing $lua_file via luajit...\n";
open my $fh, '-|', 'luajit', $harness, $lua_file
  or die "luajit: $!\n";
my $json = do { local $/; <$fh> };
close $fh;
die "luajit harness failed (exit $?)\n" if $?;
die "harness produced no output\n" unless length $json;

open my $out, '>', $out_file or die "write $out_file: $!\n";
print {$out} $json;
close $out;

print "Wrote $out_file (" . length($json) . " bytes)\n";
