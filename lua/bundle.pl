#!/usr/bin/env perl
#
# bundle.pl — generates a single, self-contained per-model control
# script: concatenates the PureLua::* engine modules and ac-ng.pl's own
# code (with its external PureLua lookup stripped out), then appends the
# target device's .lua script verbatim after __DATA__. The output is one
# file with no dependencies outside the standard library — no
# lua/PureLua/*, no --lua flag, nothing but perl needed to run it.
#
use strict;
use warnings;

use FindBin ();

my ( $device_lua, $out_path ) = @ARGV;
die "usage: bundle.pl <device.lua> <output.pl>\n" unless $device_lua && $out_path;

my $lua_dir    = $FindBin::Bin;              # lua/
my $ac_ng_path = "$lua_dir/../ac-ng.pl";

# Topological order: Table has no PureLua deps; Cjson/Eval depend on
# Table; Parser depends on Lexer; Stdlib depends on Table/Pattern/Cjson.
# (Load order only matters here because concatenation has no `use` to
# resolve it — ac-ng.pl's own `use PureLua::Parser/Eval/Stdlib;` lines
# never needed to spell out the transitive deps those modules' own `use`
# lines already covered.)
my @pure_lua_modules = qw[Table Lexer Pattern Cjson Parser Stdlib Eval];

my $bundle = "#!/usr/bin/env perl\nuse strict;\nuse warnings;\n\n";

for my $mod (@pure_lua_modules) {
    my $src = slurp("$lua_dir/PureLua/$mod.pm");
    $src = strip_internal_use( $src, qr/^use PureLua::\w+;\n/m );
    $bundle .= "$src\n";
}

my $ac_ng_src = slurp($ac_ng_path);
$ac_ng_src = rewrite_ac_ng($ac_ng_src);
$bundle .= $ac_ng_src;

my $device_src = slurp($device_lua);

open my $out_fh, '>', $out_path or die "open $out_path: $!\n";
print {$out_fh} $bundle;
print {$out_fh} "__DATA__\n";
print {$out_fh} $device_src;
close $out_fh;
chmod 0755, $out_path;

my $total_len = length($bundle) + length($device_src);
print "Wrote $out_path ($total_len bytes: ", length($bundle),
  " perl + $device_lua embedded)\n";

system( $^X, '-c', $out_path ) == 0
  or die "bundled script failed its own perl -c check — not written out as valid\n";

exit 0;

sub slurp {
    my ($path) = @_;
    open my $fh, '<', $path or die "open $path: $!\n";
    local $/;
    return <$fh>;
}

sub strip_internal_use {
    my ( $src, $pattern ) = @_;
    $src =~ s/$pattern//g;
    return $src;
}

# Rewrites ac-ng.pl's source for standalone bundling:
#   - drop its own `use FindBin`/`use lib .../PureLua`/`use PureLua::*`
#     (the modules are concatenated above instead, so there's nothing
#     left to find on @INC)
#   - drop the `--lua <path>` flag and its required-option check
#   - read the embedded device script from DATA instead of a file
# Each substitution asserts its expected hit count rather than silently
# doing nothing if ac-ng.pl's text ever drifts out from under this script
# — a bundler that quietly stops rewriting is worse than one that dies.
sub rewrite_ac_ng {
    my ($src) = @_;

    $src = assert_sub( $src, 1,
        qr/use FindBin +\(\);\nuse lib "\$FindBin::Bin\/lua";\n\nuse PureLua::Parser;\nuse PureLua::Eval;\nuse PureLua::Stdlib;\n/,
        "# PureLua::* is bundled directly above — no external lookup needed.\n"
    );

    $src = assert_sub( $src, 1,
        qr/'ip=s', 'lua=s', 'get', 'set', 'debug', 'help',/,
        q{'ip=s', 'get', 'set', 'debug', 'help',}
    );

    $src = assert_sub( $src, 1,
        qr/if \( \$opt\{help\} \|\| !\$opt\{ip\} \|\| !\$opt\{lua\} \) \{/,
        q#if ( $opt{help} || !$opt{ip} ) {#
    );

    $src = assert_sub(
        $src, 1,
        qr/Usage:\n  ac-ng\.pl --ip <addr> --lua <model\.lua> --get\n  ac-ng\.pl --ip <addr> --lua <model\.lua> --set --power on --mode cool --temperature 24\n\n--lua is the Midea-cloud-issued, device-model-specific protocol script for\nyour device \(as downloaded by ac-check\.pl --lua\)\. This tool runs its real\njsonToData\/dataToJson through a pure-Perl Lua interpreter instead of\nguessing byte offsets\./,
        <<'USAGE'
Usage:
  ac-ng.pl --ip <addr> --get
  ac-ng.pl --ip <addr> --set --power on --mode cool --temperature 24

This is a bundled, self-contained control script for one specific device
model: its protocol driver (a Midea-cloud-issued .lua script) is embedded
below the __DATA__ marker in this same file, run through a pure-Perl Lua
interpreter — no external files, no --lua flag.
USAGE
    );

    $src = assert_sub(
        $src, 1,
        qr/my \$device_src = do \{\n        open my \$fh, '<', \$opt\{lua\} or die "open \$opt\{lua\}: \$!\\n";\n        local \$\/;\n        <\$fh>;\n    \};/,
        <<'PERL'
    my $device_src = do {
        binmode(DATA);
        local $/;
        <DATA>;
    };
PERL
    );

    $src = assert_sub( $src, 1,
        qr/next if \$k =~ \/\^\(ip\|lua\|get\|set\|debug\|help\)\$\/;/,
        q{next if $k =~ /^(ip|get|set|debug|help)$/;}
    );

    return $src;
}

sub assert_sub {
    my ( $src, $expect_n, $pattern, $replacement ) = @_;
    my $n = ( $src =~ s/$pattern/$replacement/g );
    die "bundle.pl: expected $expect_n match(es) for /$pattern/, got $n — ac-ng.pl's source has drifted, update bundle.pl's rewrite rules\n"
      unless $n == $expect_n;
    return $src;
}
