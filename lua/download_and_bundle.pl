#!/usr/bin/env perl
#
# download_and_bundle.pl — end-to-end convenience wrapper around two
# already-independent tools: downloads this account's device-specific
# Midea cloud protocol script(s) (ac-check.pl --lua) and immediately
# feeds each one into bundle.pl to produce a self-contained control
# script. No new protocol or bundling logic lives here.
#
# With --device-id: bundles that one appliance. Without it: logs in,
# calls ac-check.pl's own list_appliances() directly (structured data,
# not a table to scrape back out of printed text — see the `require`
# below) to find every 0xAC appliance on the account, and bundles each.
#
use strict;
use warnings;

use FindBin      ();
use Cwd          ();
use File::Path   ();
use Getopt::Long ();

my %opt;
Getopt::Long::GetOptions(
    \%opt,
    'device-id=s',
    'region=s',
    'out=s',
    'help',
) or usage(2);

usage(0) if $opt{help};
usage(2) unless $opt{out};

die "need MIDEA_EMAIL/MIDEA_PASSWORD in the environment (same as ac-check.pl)\n"
  unless $ENV{MIDEA_EMAIL} && $ENV{MIDEA_PASSWORD};

my $orig_dir = Cwd::getcwd();
my $lua_dir  = $FindBin::Bin;              # lua/
my $root     = "$lua_dir/..";              # project root
my $ac_check = "$root/ac-check.pl";
my $bundler  = "$lua_dir/bundle.pl";

# $opt{out} is resolved against the directory this was invoked from,
# before the chdir below changes what "." means.
my $out_dir = to_abs_path( $opt{out}, $orig_dir );
File::Path::make_path($out_dir) unless -d $out_dir;

# ac-check.pl's --lua always writes the downloaded script to its own
# current directory under a server-supplied name — run from
# lua/ so it lands next to the engine and bundle.pl, consistent
# with where this project already keeps cloud-downloaded protocol
# scripts. Credentials are read from the environment by ac-check.pl
# itself (never passed as argv, so they never show up in a process
# listing).
chdir $lua_dir or die "chdir $lua_dir: $!\n";

if ( $opt{'device-id'} ) {
    my $path = fetch_and_bundle( $opt{'device-id'} );
    print "Done: $path\n";
}
else {
    # `require`, not a subprocess + text scrape: ac-check.pl's own
    # unless(caller) guard exists precisely so list_appliances() can be
    # called directly and return structured rows, instead of parsing its
    # printed --list table back apart.
    require $ac_check;

    print "Logging in to find this account's appliances...\n";
    eval { cloud_login( $ENV{MIDEA_EMAIL}, $ENV{MIDEA_PASSWORD}, region => $opt{region} ); 1 }
      or die "login failed: $@";

    my $rows = list_appliances();
    my @ac_rows = grep { normalize_device_type( $_->{type} ) eq '0xac' } @$rows;
    die "no 0xAC-type appliances found on this account (", scalar(@$rows), " total appliance(s))\n"
      unless @ac_rows;

    print 'Found ', scalar(@ac_rows), ' AC appliance(s): ',
      join( ', ', map { "$_->{id} ($_->{name})" } @ac_rows ), "\n";

    my @done;
    for my $r (@ac_rows) {
        print "--- appliance $r->{id} ($r->{name}) ---\n";
        my $path = eval { fetch_and_bundle( $r->{id} ) };
        if ($@) {
            warn "appliance $r->{id} failed, skipping: $@";
            next;
        }
        push @done, $path;
    }

    printf "Bundled %d of %d appliance(s):\n", scalar(@done), scalar(@ac_rows);
    print "  $_\n" for @done;
    exit( @done ? 0 : 1 );
}

# Downloads one appliance's protocol script (via a fresh ac-check.pl
# subprocess — its own login, kept separate from the in-process
# cloud_login() above used only for discovery) and bundles it. Returns
# the bundled script's path.
sub fetch_and_bundle {
    my ($device_id) = @_;

    my @cmd = ( $^X, $ac_check, '--device-id', $device_id, '--lua' );
    push @cmd, '--region', $opt{region} if $opt{region};

    print "Downloading protocol script for appliance $device_id...\n";

    # List-form pipe open: no shell, no quoting to get wrong. stderr is
    # left to inherit this process's own (so login/network errors show
    # up live, not just after the fact), only stdout is captured, to
    # find the "Saved: <file>" line ac-check.pl prints on success.
    open my $fh, '-|', @cmd or die "failed to run ac-check.pl: $!\n";
    my $device_lua;
    while ( my $line = <$fh> ) {
        print $line;
        $device_lua = $1 if $line =~ /^Saved: (.+)$/;
    }
    close $fh;
    my $status = $?;

    die "ac-check.pl --lua failed for appliance $device_id (exit status $status) — see output above\n"
      if $status != 0;
    die "couldn't find a \"Saved: <file>\" line in ac-check.pl's output for appliance $device_id\n"
      unless defined $device_lua && length $device_lua;

    my $device_lua_path = "$lua_dir/$device_lua";
    die "ac-check.pl reported saving \"$device_lua\", but it's not there: $device_lua_path\n"
      unless -f $device_lua_path;

    ( my $base = $device_lua ) =~ s/\.lua$//i;
    my $out_path = "$out_dir/ac-$base.pl";

    print "Bundling $device_lua -> $out_path...\n";
    system( $^X, $bundler, $device_lua_path, $out_path ) == 0
      or die "bundle.pl failed for appliance $device_id\n";

    return $out_path;
}

sub to_abs_path {
    my ( $path, $base ) = @_;
    return $path if $path =~ m{^/};
    return "$base/$path";
}

sub usage {
    my ($code) = @_;
    print STDERR <<'USAGE';
Usage:
  download_and_bundle.pl --out <dir> --device-id ID [--region R]
  download_and_bundle.pl --out <dir> [--region R]

Without --device-id, discovers every 0xAC appliance on this account and
bundles each one. Each bundle is named after its downloaded protocol
script (ac-<script-basename>.pl) inside --out, which is created if
missing.

Env: MIDEA_EMAIL, MIDEA_PASSWORD (required — same account ac-check.pl uses)

  --device-id   cloud appliance id (from ac-check.pl --list); bundles
                just this one appliance if given
  --out         directory to write the bundled control script(s) into
  --region      us|eu|prod|ru|jp (optional; omit to auto-probe)
USAGE
    exit( $code // 1 );
}
