#!/usr/bin/env perl
#
# ac-cloud.pl — Midea SmartHome cloud + LAN V3 (0x8370) control
#
# Self-contained: Perl + openssl + curl (no CPAN).
# Credentials: --user/--pass or MIDEA_EMAIL / MIDEA_PASSWORD (never hardcoded).
#
# Flow:
#   1) Cloud login (same as ac-check.pl / midealan SmartHomeCloud)
#   2) Optional --list / --token (iot/secure/getToken)
#   3) V3 handshake with device using token/key, then status query
#
# Use ac.pl for V2 default-key LAN. This tool is for V3 (0x8370) modules
# that need a cloud token/key (or --preset-keys).
#

use strict;
use warnings;
use Getopt::Long   ();
use POSIX          ();
use File::Temp     ();
use MIME::Base64   ();
use IO::Socket     ();
use Socket         ();

use constant {
    APP_ID   => '1010',
    APP_KEY  => 'ac21b9f9cbfe4ca5a88562ef25e2b768',
    IOT_KEY  => 'meicloud',
    HMAC_KEY => 'PROD_VnoClJI9aikS8dyy',
    PORT     => 6444,
    TIMEOUT  => 8,

    # 8370 msg types (msmart / midealan)
    MSG_HANDSHAKE_REQ => 0x0,
    MSG_HANDSHAKE_RSP => 0x1,
    MSG_ENC_REQUEST   => 0x6,
    MSG_ENC_RESPONSE  => 0x3,

    # Preset cloud keys (midealan DEFAULT_KEYS method 99) — last-resort V3 try
    PRESET_TOKEN =>
'ee755a84a115703768bcc7c6c13d3d629aa416f1e2fd798beb9f78cbb1381d091cc245d7b063aad2a900e5b498fbd936c811f5d504b2e656d4f33b3bbc6d1da3',
    PRESET_KEY =>
'ed37bd31558a4b039aaf4e7a7a59aa7a75fd9101682045f69baf45d28380ae5c',
};

our @MAS_HOSTS = (
    'https://mp-us-prod.appsmb.com',
    'https://mp-eu-prod.appsmb.com',
    'https://mp-prod.appsmb.com',
    'https://mp-ru-prod.appsmb.com',
    'https://mp-jp-prod.appsmb.com',
);

our $DEBUG        = 0;
our $API_BASE     = '';
our $ACCESS_TOKEN = '';    # mdata.accessToken (short) for HTTP
our $UID          = '';
our $DEVICE_ID    = '';
our $REQ_COUNT    = 0;
our $TCP_KEY;    # binary 16 bytes after handshake

sub dbg {
    return unless $DEBUG;
    my ( $l, $t ) = @_;
    $t = '' unless defined $t;
    $t =~ s/(accessToken|password|iampwd|token|key|sign)("?\s*[:=]\s*"?)[^"&\s,}]+/$1$2***/gi;
    printf STDERR "[DEBUG] %-18s %s\n", $l, $t;
}

sub die_usage {
    print STDERR <<"USAGE";
Usage:
  ac-cloud.pl --user EMAIL --pass PASS --list
  ac-cloud.pl --user EMAIL --pass PASS --device-id ID --token
  ac-cloud.pl --ip HOST --device-id ID --user EMAIL --pass PASS --get
  ac-cloud.pl --ip HOST --token HEX --key HEX --get

  --list           list cloud appliances
  --token          fetch V3 token/key for --device-id (prints hex)
  --get            V3 authenticate + status query
  --preset-keys    try midealan preset token/key (no cloud) with --ip --get
  --region R       us|eu|prod|ru|jp (skip auto-probe)

Env: MIDEA_EMAIL, MIDEA_PASSWORD
Requires: openssl, curl
USAGE
    exit( $_[0] // 1 );
}

sub region_host {
    my ($r) = @_;
    $r = lc( $r // '' );
    return 'https://mp-us-prod.appsmb.com' if $r eq 'us';
    return 'https://mp-eu-prod.appsmb.com' if $r eq 'eu';
    return 'https://mp-prod.appsmb.com'    if $r eq 'prod' || $r eq 'global';
    return 'https://mp-ru-prod.appsmb.com' if $r eq 'ru';
    return 'https://mp-jp-prod.appsmb.com' if $r eq 'jp';
    return undef;
}

sub set_api_base {
    my ($host) = @_;
    $host =~ s{/mas/v5/app/proxy\?alias=/?$}{};
    $host =~ s{/$}{};
    $API_BASE = $host . '/mas/v5/app/proxy?alias=';
    dbg( 'api_base', $API_BASE );
}

# --- openssl helpers ---

sub _dgst_hex {
    my ( $alg, $data, @ex ) = @_;
    my $tmp = File::Temp->new( UNLINK => 1 );
    binmode $tmp;
    print {$tmp} $data;
    $tmp->flush;
    open my $fh, '-|', 'openssl', 'dgst', "-$alg", @ex, $tmp->filename
      or die "openssl: $!\n";
    my $o = do { local $/; <$fh> };
    close $fh;
    $o =~ /=\s*([0-9a-fA-F]+)/ or die "openssl dgst: $o\n";
    return lc $1;
}

sub _dgst_bin {
    my ( $alg, $data, @ex ) = @_;
    my $tmp = File::Temp->new( UNLINK => 1 );
    binmode $tmp;
    print {$tmp} $data;
    $tmp->flush;
    open my $fh, '-|', 'openssl', 'dgst', "-$alg", '-binary', @ex, $tmp->filename
      or die "openssl: $!\n";
    binmode $fh;
    my $o = do { local $/; <$fh> };
    close $fh;
    return $o;
}

sub sha256_hex { _dgst_hex( 'sha256', $_[0] ) }
sub sha256_bin { _dgst_bin( 'sha256', $_[0] ) }
sub md5_hex    { _dgst_hex( 'md5',    $_[0] ) }
sub hmac_sha256_hex {
    _dgst_hex( 'sha256', $_[0], '-hmac', $_[1] );
}

sub encrypt_password {
    my ( $lid, $pw ) = @_;
    return sha256_hex( $lid . sha256_hex($pw) . APP_KEY );
}

sub encrypt_iam_password {
    my ( $lid, $pw ) = @_;
    return sha256_hex( $lid . md5_hex( md5_hex($pw) ) . APP_KEY );
}

sub cloud_device_id {
    return substr( sha256_hex( "Hello, $_[0]!" ), 0, 16 );
}

sub stamp  { POSIX::strftime( '%Y%m%d%H%M%S', gmtime ) }
sub req_id { unpack( 'H*', join '', map { chr rand 256 } 1 .. 16 ) }
sub auth_basic {
    MIME::Base64::encode_base64( APP_KEY . ':' . IOT_KEY, '' );
}

sub json_escape {
    my ($s) = @_;
    $s = '' unless defined $s;
    $s =~ s/\\/\\\\/g;
    $s =~ s/"/\\"/g;
    return $s;
}

sub json_encode {
    my ($v) = @_;
    return 'null' if !defined $v;
    if ( ref $v eq 'HASH' ) {
        return '{'
          . join( ',',
            map { '"' . json_escape($_) . '":' . json_encode( $v->{$_} ) }
            sort keys %$v )
          . '}';
    }
    if ( ref $v eq 'ARRAY' ) {
        return '[' . join( ',', map { json_encode($_) } @$v ) . ']';
    }
    return '"' . json_escape($v) . '"';
}

sub json_get {
    my ( $j, $k ) = @_;
    return $1 if $j =~ /"$k"\s*:\s*"((?:\\.|[^"\\])*)"/;
    return $1 if $j =~ /"$k"\s*:\s*(-?[0-9]+(?:\.[0-9]+)?)/;
    return undef;
}

sub json_code {
    my ($j) = @_;
    my $c = json_get( $j, 'code' );
    $c = json_get( $j, 'errorCode' ) unless defined $c;
    return defined $c ? 0 + $c : -1;
}

sub https_json {
    my ( $alias, $body ) = @_;
    die "API base not set\n" unless length $API_BASE;
    my $json   = json_encode($body);
    my $random = req_id();
    my $sign   = hmac_sha256_hex( IOT_KEY . $json . $random, HMAC_KEY );
    my $url    = $API_BASE . $alias;
    dbg( 'POST', $url );
    dbg( 'body', $json );
    my @cmd = (
        'curl', '-sS', '-m', '20', '-X', 'POST', $url,
        '-H', 'Content-Type: application/json; charset=utf-8',
        '-H', 'secretVersion: 1',
        '-H', "sign: $sign",
        '-H', "random: $random",
        '-H', 'x-recipe-app: ' . APP_ID,
        '-H', 'authorization: Basic ' . auth_basic(),
        '--data-binary', $json,
    );
    push @cmd, '-H', "accessToken: $ACCESS_TOKEN" if length $ACCESS_TOKEN;
    push @cmd, '-H', "uid: $UID" if length $UID;
    open my $fh, '-|', @cmd or die "curl: $!\n";
    my $resp = do { local $/; <$fh> };
    close $fh;
    dbg( 'response', $resp // '' );
    return $resp // '';
}

sub general_data {
    return {
        src        => APP_ID,
        format     => '2',
        stamp      => stamp(),
        platformId => '1',
        deviceId   => $DEVICE_ID,
        reqId      => req_id(),
        ( length($UID) ? ( uid => $UID ) : () ),
        clientType => '1',
        appId      => APP_ID,
        language   => 'en_US',
    };
}

sub resolve_region {
    my ( $account, $region ) = @_;
    if ( defined $region && length $region ) {
        my $h = region_host($region) // die "unknown --region $region\n";
        set_api_base($h);
        print "Region: $region ($h)\n";
        return;
    }
    for my $h (@MAS_HOSTS) {
        set_api_base($h);
        print "Probing $h ... ";
        my $g = general_data();
        $g->{loginAccount} = $account;
        my $r = https_json( '/v1/user/login/id/get', $g );
        if ( json_code($r) == 0 && json_get( $r, 'loginId' ) ) {
            print "loginId found\n";
            return;
        }
        print "no\n";
    }
    die "Account not found on any regional MAS\n";
}

sub cloud_login {
    my ( $account, $password, %opt ) = @_;
    $DEVICE_ID = cloud_device_id($account);
    resolve_region( $account, $opt{region} );
    {
        my $g = general_data();
        $g->{userType} = '0';
        $g->{userName} = $account;
        https_json( '/v1/multicloud/platform/user/route', $g );
    }
    my $g = general_data();
    $g->{loginAccount} = $account;
    my $r1 = https_json( '/v1/user/login/id/get', $g );
    die "login/id/get failed: $r1\n" if json_code($r1) != 0;
    my $login_id = json_get( $r1, 'loginId' ) // die "no loginId\n";

    my $iot = general_data();
    delete $iot->{uid};
    $iot->{loginAccount} = $account;
    $iot->{password}     = encrypt_password( $login_id, $password );
    $iot->{iampwd}       = encrypt_iam_password( $login_id, $password );
    $iot->{stamp}        = stamp();
    my $body = {
        iotData => $iot,
        data    => { appKey => APP_KEY, deviceId => $DEVICE_ID, platform => '2' },
        stamp   => $iot->{stamp},
    };
    my $r2 = https_json( '/mj/user/login', $body );
    die "login failed: $r2\n" if json_code($r2) != 0;
    $UID = json_get( $r2, 'uid' ) // '';
    if ( $r2 =~ /"mdata"\s*:\s*\{.*?"accessToken"\s*:\s*"([^"]+)"/s ) {
        $ACCESS_TOKEN = $1;
    }
    elsif ( $r2 =~ /"accessToken"\s*:\s*"(T1[^"]+)"/ ) {
        $ACCESS_TOKEN = $1;
    }
    else {
        die "no mdata.accessToken: $r2\n";
    }
    return 1;
}

sub list_appliances {
    my $raw = https_json( '/v1/appliance/user/list/get', general_data() );
    die "list failed: $raw\n" if json_code($raw) != 0;
    my @rows;
    while ( $raw =~ /\{([^{}]*"id"\s*:\s*"?\d+"?[^{}]*)\}/g ) {
        my $c  = '{' . $1 . '}';
        my $id = json_get( $c, 'id' );
        next unless defined $id;
        push @rows,
          {
            id   => $id,
            name => json_get( $c, 'name' ) // '',
            type => json_get( $c, 'type' ) // '',
            on   => json_get( $c, 'onlineStatus' ) // '',
          };
    }
    return \@rows;
}

sub get_udp_id {
    my ( $appliance_id, $method ) = @_;
    # midealan UdpIdMethod: 0=REVERSED_BIG(8), 1=BIG(6), 2=LITTLE(6)
    my $bytes;
    if ( $method == 0 ) {
        $bytes = reverse pack( 'Q>', $appliance_id + 0 );
    }
    elsif ( $method == 1 ) {
        $bytes = pack( 'H*', sprintf( '%012x', $appliance_id + 0 ) );
    }
    elsif ( $method == 2 ) {
        $bytes = reverse pack( 'H*', sprintf( '%012x', $appliance_id + 0 ) );
    }
    else {
        return undef;
    }
    my $digest = sha256_bin($bytes);
    my @d = unpack( 'C*', $digest );
    for my $i ( 0 .. 15 ) {
        $d[$i] ^= $d[ $i + 16 ];
    }
    return unpack( 'H*', pack( 'C*', @d[ 0 .. 15 ] ) );
}

sub get_cloud_token {
    my ($appliance_id) = @_;
    my %out;
    for my $method ( 1, 2 ) {    # BIG, LITTLE (midealan get_cloud_keys)
        my $udp = get_udp_id( $appliance_id, $method );
        my $g = general_data();
        $g->{udpid}          = $udp;
        $g->{applianceCodes} = "$appliance_id";
        my $raw = https_json( '/v1/iot/secure/getToken', $g );
        next if json_code($raw) != 0;
        while ( $raw =~ /\{([^{}]*"udpId"[^{}]*)\}/g ) {
            my $c = '{' . $1 . '}';
            my $u = json_get( $c, 'udpId' ) // '';
            next unless lc($u) eq lc($udp);
            $out{$method} = {
                token => lc( json_get( $c, 'token' ) // '' ),
                key   => lc( json_get( $c, 'key' )   // '' ),
            };
        }
    }
    return \%out;
}

# --- AES-128-CBC via openssl (IV = 16 zero bytes) ---

sub aes_cbc {
    my ( $enc, $key_bin, $data ) = @_;
    my $K = unpack( 'H*', $key_bin );
    my $iv = '00' x 16;
    my $in  = File::Temp->new( UNLINK => 1 );
    my $out = File::Temp->new( UNLINK => 1 );
    binmode $in;
    print {$in} $data;
    $in->flush;
    my @cmd = (
        'openssl', 'enc', '-aes-128-cbc',
        ( $enc ? () : ('-d') ),
        '-nopad', '-K', $K, '-iv', $iv,
        '-in', $in->filename, '-out', $out->filename,
    );
    system(@cmd) == 0 or die "openssl aes failed\n";
    open my $fh, '<:raw', $out->filename or die $!;
    local $/;
    my $p = <$fh>;
    close $fh;
    return $p;
}

# --- AES-128-ECB for inner V2 payload (reuse logic via openssl) ---

sub aes_ecb {
    my ( $enc, $key_bin, $data ) = @_;
    # PKCS7 pad for encrypt
    if ($enc) {
        my $pad = 16 - ( length($data) % 16 );
        $data .= chr($pad) x $pad;
    }
    my $K = unpack( 'H*', $key_bin );
    my $in  = File::Temp->new( UNLINK => 1 );
    my $out = File::Temp->new( UNLINK => 1 );
    binmode $in;
    print {$in} $data;
    $in->flush;
    my @cmd = (
        'openssl', 'enc', '-aes-128-ecb',
        ( $enc ? () : ('-d') ),
        '-nopad', '-K', $K,
        '-in', $in->filename, '-out', $out->filename,
    );
    system(@cmd) == 0 or die "openssl ecb failed\n";
    open my $fh, '<:raw', $out->filename or die $!;
    local $/;
    my $p = <$fh>;
    close $fh;
    unless ($enc) {
        my $pad = ord( substr( $p, -1 ) );
        $p = substr( $p, 0, length($p) - $pad ) if $pad > 0 && $pad <= 16;
    }
    return $p;
}

# Default LAN AES key = md5(KEY_SIGN) as in ac.pl
use constant KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S';
use constant LAN_KEY =>
  pack( 'H*', '6a92ef406bad2f0359baad994171ea6d' );

sub md5_bin {
    return pack( 'H*', md5_hex( $_[0] ) );
}

sub encode_8370 {
    my ( $data, $msgtype ) = @_;
    my $size    = length($data);
    my $padding = 0;
    if (   ( $msgtype == MSG_ENC_REQUEST || $msgtype == MSG_ENC_RESPONSE )
        && ( ( $size + 2 ) % 16 != 0 ) )
    {
        $padding = 16 - ( ( $size + 2 ) & 0x0f );
        $size += $padding + 32;
        $data .= join '', map { chr rand 256 } 1 .. $padding;
    }
    my $header = pack( 'CCnCC', 0x83, 0x70, $size, 0x20, ( $padding << 4 ) | $msgtype );
    my $payload = pack( 'n', $REQ_COUNT ) . $data;
    $REQ_COUNT = ( $REQ_COUNT + 1 ) & 0xffff;
    if ( $msgtype == MSG_ENC_REQUEST || $msgtype == MSG_ENC_RESPONSE ) {
        my $sign = sha256_bin( $header . $payload );
        $payload = aes_cbc( 1, $TCP_KEY, $payload ) . $sign;
    }
    return $header . $payload;
}

sub decode_8370_one {
    my ($pkt) = @_;
    return undef if length($pkt) < 8;
    my ( $m0, $m1, $size, $b4, $b5 ) = unpack( 'CCnCC', substr( $pkt, 0, 6 ) );
    die "not 8370\n" unless $m0 == 0x83 && $m1 == 0x70;
    my $total = $size + 8;
    die "short packet\n" if length($pkt) < $total;
    my $padding = $b5 >> 4;
    my $msgtype = $b5 & 0x0f;
    my $data    = substr( $pkt, 6, $total - 6 );
    if ( $msgtype == MSG_ENC_REQUEST || $msgtype == MSG_ENC_RESPONSE ) {
        my $sign = substr( $data, -32 );
        $data = substr( $data, 0, length($data) - 32 );
        $data = aes_cbc( 0, $TCP_KEY, $data );
        my $hdr = substr( $pkt, 0, 6 );
        die "8370 sign mismatch\n" unless sha256_bin( $hdr . $data ) eq $sign;
        $data = substr( $data, 0, length($data) - $padding ) if $padding;
    }
    # strip packet id
    return substr( $data, 2 );
}

sub v3_handshake {
    my ( $sock, $token_hex, $key_hex ) = @_;
    my $token = pack( 'H*', $token_hex );
    my $key   = pack( 'H*', $key_hex );
    my $pkt   = encode_8370( $token, MSG_HANDSHAKE_REQ );
    dbg( 'V3 HS TX', unpack( 'H*', $pkt ) );
    $sock->send($pkt) == length($pkt) or die "send hs: $!\n";

    my $buf = '';
    eval {
        local $SIG{ALRM} = sub { die "timeout\n" };
        alarm TIMEOUT;
        while ( length($buf) < 8 ) {
            my $n = $sock->sysread( my $chunk, 256 );
            die "no hs response\n" unless $n;
            $buf .= $chunk;
        }
        my $size = unpack( 'n', substr( $buf, 2, 2 ) ) + 8;
        while ( length($buf) < $size ) {
            my $n = $sock->sysread( my $chunk, 256 );
            die "short hs\n" unless $n;
            $buf .= $chunk;
        }
        alarm 0;
    };
    alarm 0;
    die "handshake: $@\n" if $@;
    dbg( 'V3 HS RX', unpack( 'H*', $buf ) );

    # response payload is 64 bytes after 8370 header for handshake
    my ( undef, undef, $size ) = unpack( 'CCn', substr( $buf, 0, 4 ) );
    my $body = substr( $buf, 6, $size );
    die "handshake ERROR\n" if $body eq 'ERROR';
    die "bad hs length " . length($body) . "\n" unless length($body) == 64;
    my $plain = aes_cbc( 0, $key, substr( $body, 0, 32 ) );
    my $sign  = substr( $body, 32, 32 );
    die "hs sign mismatch\n" unless sha256_bin($plain) eq $sign;
    $TCP_KEY = $plain ^ $key;
    $REQ_COUNT = 0;
    dbg( 'tcp_key', unpack( 'H*', $TCP_KEY ) );
    return 1;
}

sub build_status_v2 {
    # Minimal CMD_41 status query (same shape as ac.pl get_cmd), wrapped as V2 packet
    my @cmd = (
        0xaa, 0x00, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x41, 0x81, 0x00, 0xff, 0x03, 0xff,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    );
    # length + crc8 left simplistic: device often accepts fixed working packet
    # Use the known-working encrypted outer from ac.pl style instead:
    return undef;    # filled below via packet() clone
}

# Build V2 outer packet like ac.pl (AES-ECB + MD5 footer)
sub v2_status_packet {
    my $inner = pack(
        'C*',
        0xaa, 0x1d, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x41, 0x81, 0x00, 0xff, 0x03, 0xff,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00,
    );
    # pad + encrypt with LAN_KEY
    my $enc = aes_ecb( 1, LAN_KEY, $inner );
    my $hdr = pack( 'C*',
        0x5a, 0x5a, 0x01, 0x11,
        0x00, 0x00,    # length placeholder
        0x20, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    );
    # Fix: use simpler construction matching ac.pl packet() — for V3 the
    # inner encrypted V2 packet is what gets wrapped; many stacks send the
    # full 5a5a frame inside 8370.
    my $body = $hdr . $enc;
    my $len  = length($body) + 16;    # + md5
    substr( $body, 4, 2, pack( 'n', $len ) );
    my $md = md5_bin( $body . KEY_SIGN );
    return $body . $md;
}

sub v3_request {
    my ( $sock, $v2packet ) = @_;
    my $pkt = encode_8370( $v2packet, MSG_ENC_REQUEST );
    dbg( 'V3 TX', unpack( 'H*', $pkt ) );
    $sock->send($pkt) == length($pkt) or die "send: $!\n";
    my $buf = '';
    eval {
        local $SIG{ALRM} = sub { die "timeout\n" };
        alarm TIMEOUT;
        while (1) {
            my $n = $sock->sysread( my $chunk, 512 );
            die "no response\n" unless defined $n && $n > 0;
            $buf .= $chunk;
            last if length($buf) >= 8 && length($buf) >= ( unpack( 'n', substr( $buf, 2, 2 ) ) + 8 );
        }
        alarm 0;
    };
    alarm 0;
    die "recv: $@\n" if $@;
    dbg( 'V3 RX', unpack( 'H*', $buf ) );
    return decode_8370_one($buf);
}

sub tcp_connect {
    my ($ip) = @_;
    my $s = IO::Socket->new(
        Domain    => IO::Socket::AF_INET,
        Type      => IO::Socket::SOCK_STREAM,
        Proto     => 'tcp',
        Timeout   => TIMEOUT,
        PeerHost  => $ip,
        PeerPort  => PORT,
        ReusePort => 1,
    ) or die "connect $ip: $@\n";
    return $s;
}

# --- CLI ---

my %opt;
Getopt::Long::GetOptions(
    \%opt,
    'user=s', 'pass=s', 'device-id=s', 'ip=s',
    'token', 'list', 'get', 'preset-keys',
    'token-hex=s', 'key-hex=s',
    'region=s',
    'debug', 'help',
) or die_usage(2);
die_usage(0) if $opt{help};

$opt{user} //= $ENV{MIDEA_EMAIL};
$opt{pass} //= $ENV{MIDEA_PASSWORD};
$DEBUG = 1 if $opt{debug};

unless ( $opt{list} || $opt{token} || $opt{get} ) {
    $opt{list} = 1;
}

my ( $token_hex, $key_hex ) = ( $opt{'token-hex'}, $opt{'key-hex'} );

if ( $opt{'preset-keys'} ) {
    $token_hex = PRESET_TOKEN;
    $key_hex   = PRESET_KEY;
}

if ( ( $opt{list} || $opt{token} || ( $opt{get} && !$token_hex ) )
    && !( $opt{user} && $opt{pass} ) && !$opt{'preset-keys'} )
{
    die_usage(2);
}

if ( $opt{user} && $opt{pass} && !$opt{'preset-keys'} ) {
    print "Authenticating...\n";
    eval {
        cloud_login( $opt{user}, $opt{pass}, region => $opt{region} );
        1;
    } or do {
        print STDERR "Cloud login failed: $@\n";
        print STDERR
"Check MSmartLife credentials (3102 = wrong password or wrong region).\n",
          "Omit --region to auto-probe, or try --region us.\n";
        exit 1;
    };
    print "OK\n";
}

if ( $opt{list} ) {
    my $rows = list_appliances();
    printf "%-18s %-8s %-6s %s\n", 'id', 'type', 'online', 'name';
    printf "%s\n", '-' x 60;
    for my $r (@$rows) {
        printf "%-18s %-8s %-6s %s\n", $r->{id}, $r->{type},
          ( ( $r->{on} eq '1' ) ? 'yes' : 'no' ), $r->{name};
    }
    print "(empty)\n" unless @$rows;
}

if ( $opt{token} ) {
    die "--device-id required\n" unless $opt{'device-id'};
    my $toks = get_cloud_token( $opt{'device-id'} );
    if ( !keys %$toks ) {
        print "No token returned for appliance $opt{'device-id'}\n";
        exit 1;
    }
    for my $m ( sort keys %$toks ) {
        print "method $m:\n";
        print "  token: $toks->{$m}{token}\n";
        print "  key:   $toks->{$m}{key}\n";
        $token_hex //= $toks->{$m}{token};
        $key_hex   //= $toks->{$m}{key};
    }
}

if ( $opt{get} ) {
    die "--ip required\n" unless $opt{ip};
    if ( !$token_hex && $opt{'device-id'} && $ACCESS_TOKEN ) {
        my $toks = get_cloud_token( $opt{'device-id'} );
        for my $m ( sort keys %$toks ) {
            $token_hex = $toks->{$m}{token};
            $key_hex   = $toks->{$m}{key};
            last;
        }
    }
    die "need --token-hex/--key-hex or cloud --token/--device-id or --preset-keys\n"
      unless $token_hex && $key_hex;

    print "Connecting to $opt{ip}:" . PORT . " (V3)...\n";
    my $sock = tcp_connect( $opt{ip} );
    print "Handshake...\n";
    v3_handshake( $sock, $token_hex, $key_hex );
    print "Status query...\n";
    my $v2  = v2_status_packet();
    my $rsp = v3_request( $sock, $v2 );
    print "V3 response (" . length($rsp) . " bytes):\n";
    print unpack( 'H*', $rsp ), "\n";
    $sock->close();
}

exit 0;
