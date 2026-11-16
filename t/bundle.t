#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use File::Temp qw(tempdir);

my $bundler = "$Bin/../lua/bundle.pl";
my $device_script = "$Bin/../lua/T_0000_AC_00000Q1B_2024013001.lua";
plan skip_all => "device script not present: $device_script" unless -f $device_script;

my $work_dir = tempdir( CLEANUP => 1 );
my $bundled  = "$work_dir/ac-bundled.pl";

my $gen_out = `"$^X" "$bundler" "$device_script" "$bundled" 2>&1`;
is( $?, 0, 'bundle.pl exits cleanly' ) or diag($gen_out);
ok( -f $bundled, 'bundle.pl wrote the output file' );

my $src = do { open my $fh, '<', $bundled or die $!; local $/; <$fh> };

# The whole point of bundling: nothing in the output should still try to
# find a PureLua::* module on @INC, or read an external --lua file.
unlike( $src, qr/use PureLua::/, 'bundled script has no external PureLua:: use statements' );
unlike( $src, qr/\$opt\{lua\}/,   'bundled script has no leftover --lua option handling' );
like( $src, qr/\n__DATA__\n/, 'bundled script has a __DATA__ section' );

my ($data_section) = $src =~ /\n__DATA__\n(.*)/s;
my $device_src = do { open my $fh, '<', $device_script or die $!; local $/; <$fh> };
is( $data_section, $device_src, 'the embedded device script is byte-identical to the source .lua' );

# Run it from a directory that has no lua/PureLua/* and no
# ac-ng.pl nearby — the only real test of "self-contained".
my $isolated_dir = tempdir( CLEANUP => 1 );
my $isolated_copy = "$isolated_dir/ac-bundled.pl";
{
    open my $in,  '<', $bundled       or die $!;
    open my $out, '>', $isolated_copy or die $!;
    local $/;
    print {$out} <$in>;
}
chmod 0755, $isolated_copy;

my $help_out = `cd "$isolated_dir" && "$^X" ac-bundled.pl --help 2>&1`;
is( $?, 0, 'bundled script runs --help standalone, isolated from the project tree' );
like( $help_out, qr/self-contained control script/, 'bundled script --help describes itself as bundled' );

done_testing;
