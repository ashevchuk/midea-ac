#!/usr/bin/env perl
#
# ac-check.pl — Midea SmartHome cloud iCheck / AC diagnostics
#
# Self-contained: needs only Perl + openssl + curl (no CPAN modules).
# Credentials via --user/--pass or env MIDEA_EMAIL / MIDEA_PASSWORD
# (never hardcode secrets in this file).
#
# Cloud: SmartHome / MSmartLife overseas, appId 1010.
# Auto-probes regional MAS hosts unless --region is set.
#
# iCheck is NOT a direct MAS alias. The app wraps plugin calls as:
#   POST .../proxy?alias=/v1/app2base/data/transmit
#   param.serviceUrl = /orac-check/icheck/...
#   data = AES-128-CBC(JSON payload) with session dataKey/dataIv
# Session keys: daesWithKey(accessToken|randomData, appKey) —
#   AES-CBC decrypt with key/iv = sha256(appKey) hex halves (ASCII).
#

use strict;
use warnings;
use Getopt::Long  ();
use POSIX         ();
use File::Temp    ();
use MIME::Base64  ();

use constant {
    APP_ID   => '1010',
    APP_KEY  => 'ac21b9f9cbfe4ca5a88562ef25e2b768',
    IOT_KEY  => 'meicloud',
    HMAC_KEY => 'PROD_VnoClJI9aikS8dyy',
};

# Regional MAS hosts (host only; path appended in https_json)
our @MAS_HOSTS = (
    'https://mp-us-prod.appsmb.com',
    'https://mp-eu-prod.appsmb.com',
    'https://mp-prod.appsmb.com',
    'https://mp-ru-prod.appsmb.com',
    'https://mp-jp-prod.appsmb.com',
);

our $DEBUG = 0;
our $API_BASE     = '';    # set after region resolve
our $ACCESS_TOKEN = '';    # HTTP header: data.mdata.accessToken (short)
our $AES_TOKEN    = '';    # data.accessToken (long hex ciphertext)
our $AES_IV       = '';    # data.randomData (long hex ciphertext)
our $DATA_KEY     = '';    # 16-char ASCII session AES key (daesWithKey)
our $DATA_IV      = '';    # 16-char ASCII session AES IV
our $UID          = '';
our $HOME_ID      = '';    # homegroupId for app2base transmit
our $DEVICE_ID    = '';    # cloud "deviceId" derived from account (16 hex)
our $TX_DEVICE_ID = '';    # 32-hex deviceId used in transmit envelope

sub dbg {
    return unless $DEBUG;
    my ( $label, $text ) = @_;
    $text = '' unless defined $text;
    $text =~ s/(accessToken|password|iampwd|sign)("?\s*[:=]\s*"?)[^"&\s,}]+/$1$2***/gi;
    printf STDERR "[DEBUG] %-18s %s\n", $label, $text;
}

sub die_usage {
    print STDERR <<"USAGE";
Usage:
  ac-check.pl --user EMAIL --pass PASS [--list]
  ac-check.pl --user EMAIL --pass PASS --device-id ID --start [--more]
  ac-check.pl --user EMAIL --pass PASS --device-id ID --history
  ac-check.pl --user EMAIL --pass PASS --device-id ID --history-id ID --details

Env: MIDEA_EMAIL, MIDEA_PASSWORD (used when --user/--pass omitted)

  --list          list appliances (id / online / sn)
  --device-id     cloud appliance id (from --list)
  --start         run new iCheck, wait for historyId, then --details
  --details       fault details (history/detail if --history-id set)
  --more          full OK/FAULT catalog (checkMore; needs historyId)
  --history       past checks (time, historyId, result text)
  --summary       checkSummary counts only
  --history-id ID id from --history or from --start output
  --region R      us|eu|prod|ru|jp  (optional; omit to auto-probe)
  --poll SEC      wait budget for historyId [default: 15]
  --debug         raw request/response on stderr
  --help

Requires: openssl, curl

Note: iCheck goes through /v1/app2base/data/transmit (not a direct alias).
USAGE
    exit( $_[0] // 1 );
}

sub region_host {
    my ($r) = @_;
    $r = lc( $r // '' );
    return 'https://mp-us-prod.appsmb.com'  if $r eq 'us';
    return 'https://mp-eu-prod.appsmb.com'  if $r eq 'eu';
    return 'https://mp-prod.appsmb.com'     if $r eq 'prod' || $r eq 'global';
    return 'https://mp-ru-prod.appsmb.com'  if $r eq 'ru';
    return 'https://mp-jp-prod.appsmb.com'  if $r eq 'jp';
    return undef;
}

sub set_api_base {
    my ($host) = @_;
    $host =~ s{/mas/v5/app/proxy\?alias=/?$}{};
    $host =~ s{/$}{};
    $API_BASE = $host . '/mas/v5/app/proxy?alias=';
    dbg( 'api_base', $API_BASE );
}

# --- crypto via openssl (no CPAN Digest::*) ---

sub _openssl_hex {
    my ( $alg, $data, @extra ) = @_;
    my $tmp = File::Temp->new( UNLINK => 1 );
    binmode $tmp;
    print {$tmp} $data;
    $tmp->flush;
    my @cmd = ( 'openssl', 'dgst', "-$alg", @extra, $tmp->filename );
    open my $fh, '-|', @cmd or die "openssl dgst: $!\n";
    my $out = do { local $/; <$fh> };
    close $fh;
    $out =~ /=\s*([0-9a-fA-F]+)/ or die "bad openssl output: $out\n";
    return lc $1;
}

sub sha256_hex { return _openssl_hex( 'sha256', $_[0] ) }
sub md5_hex    { return _openssl_hex( 'md5',    $_[0] ) }

sub hmac_sha256_hex {
    my ( $data, $key ) = @_;
    return _openssl_hex( 'sha256', $data, '-hmac', $key );
}

sub encrypt_password {
    my ( $login_id, $password ) = @_;
    my $inner = sha256_hex($password);
    return sha256_hex( $login_id . $inner . APP_KEY );
}

sub encrypt_iam_password {
    my ( $login_id, $password ) = @_;
    my $inner = md5_hex( md5_hex($password) );
    return sha256_hex( $login_id . $inner . APP_KEY );
}

sub cloud_device_id {
    my ($account) = @_;
    return substr( sha256_hex("Hello, $account!"), 0, 16 );
}

sub stamp {
    return POSIX::strftime( '%Y%m%d%H%M%S', gmtime );
}

sub req_id {
    return unpack( 'H*', join '', map { chr( int( rand(256) ) ) } 1 .. 16 );
}

sub millis_random {
    return int( time() * 1000 );
}

sub auth_basic {
    return MIME::Base64::encode_base64( APP_KEY . ':' . IOT_KEY, '' );
}

# PKCS7 pad then AES-128-CBC via openssl (-nopad; we pad ourselves).
sub aes128_cbc {
    my ( $encrypt, $key_ascii, $iv_ascii, $data ) = @_;
    my $key_hex = unpack( 'H*', substr( $key_ascii . ( "\0" x 16 ), 0, 16 ) );
    my $iv_hex  = unpack( 'H*', substr( $iv_ascii .  ( "\0" x 16 ), 0, 16 ) );

    if ($encrypt) {
        my $pad = 16 - ( length($data) % 16 );
        $data .= chr($pad) x $pad;
    }

    my $in  = File::Temp->new( UNLINK => 1 );
    my $out = File::Temp->new( UNLINK => 1 );
    binmode $in;
    binmode $out;
    print {$in} $data;
    $in->flush;

    my @cmd = (
        'openssl', 'enc', '-aes-128-cbc',
        ( $encrypt ? '-e' : '-d' ),
        '-nopad',
        '-K', $key_hex,
        '-iv', $iv_hex,
        '-in', $in->filename,
        '-out', $out->filename,
    );
    system(@cmd) == 0 or die "openssl aes-128-cbc failed\n";
    open my $fh, '<:raw', $out->filename or die $!;
    local $/;
    my $p = <$fh>;
    close $fh;

    unless ($encrypt) {
        my $pad = ord( substr( $p, -1 ) );
        $p = substr( $p, 0, -$pad ) if $pad >= 1 && $pad <= 16;
    }
    return $p;
}

# EncodeAndDecodeUtils.daesWithKey(cipherHex, appKey):
#   tmp = sha256(appKey) hex; AES-CBC decrypt cipher with key=tmp[0:16], iv=tmp[16:32]
sub daes_with_key {
    my ($cipher_hex) = @_;
    die "daes_with_key: bad cipher\n"
      unless defined $cipher_hex && $cipher_hex =~ /^[0-9a-fA-F]+$/ && length($cipher_hex) >= 32;
    my $digest = sha256_hex(APP_KEY);
    my $tmp_key = substr( $digest, 0,  16 );
    my $tmp_iv  = substr( $digest, 16, 16 );
    return aes128_cbc( 0, $tmp_key, $tmp_iv, pack( 'H*', $cipher_hex ) );
}

sub derive_session_aes {
    die "missing login accessToken/randomData\n"
      unless length($AES_TOKEN) >= 32 && length($AES_IV) >= 32;
    $DATA_KEY = daes_with_key($AES_TOKEN);
    $DATA_IV  = daes_with_key($AES_IV);
    dbg( 'dataKey', $DATA_KEY );
    dbg( 'dataIv',  $DATA_IV );
}

# --- HTTPS via curl ---

sub https_json {
    my ( $alias, $body_hash, %opt ) = @_;

    die "API base not set (region not resolved)\n" unless length $API_BASE;

    my $json = json_encode($body_hash);
    my $random = $opt{random} // req_id();
    my $sign   = hmac_sha256_hex( IOT_KEY . $json . $random, HMAC_KEY );
    my $url    = $API_BASE . $alias;

    dbg( 'POST', $url );
    dbg( 'body', $json );

    my @cmd = (
        'curl', '-sS', '-m', '60',
        '-X', 'POST', $url,
        '-H', 'Content-Type: application/json; charset=utf-8',
        '-H', 'secretVersion: 1',
        '-H', "sign: $sign",
        '-H', "random: $random",
        '-H', 'x-recipe-app: ' . APP_ID,
        '-H', 'authorization: Basic ' . auth_basic(),
        '-H', 'sourceSystem: mjapp',
        '-H', 'userUnitRegion: 1',
        '-H', 'Accept-Encoding: identity',
        '-H', 'version: 3.18.0',
        '-H', 'systemVersion: 10',
        '-H', 'platform: 0',
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

# Minimal JSON (objects with string/number/bool/null/array values we need)

sub json_escape {
    my ($s) = @_;
    $s = '' unless defined $s;
    $s =~ s/\\/\\\\/g;
    $s =~ s/"/\\"/g;
    $s =~ s/\n/\\n/g;
    $s =~ s/\r/\\r/g;
    $s =~ s/\t/\\t/g;
    return $s;
}

sub json_encode {
    my ($v) = @_;
    if ( !defined $v ) {
        return 'null';
    }
    elsif ( ref $v eq 'HASH' ) {
        return '{'
          . join( ',',
            map { '"' . json_escape($_) . '":' . json_encode( $v->{$_} ) }
            sort keys %$v )
          . '}';
    }
    elsif ( ref $v eq 'ARRAY' ) {
        return '[' . join( ',', map { json_encode($_) } @$v ) . ']';
    }
    elsif ( ref $v eq 'SCALAR' ) {
        # \0 = number, \1 = bool true, \2 = bool false
        return ${$v};
    }
    else {
        # Always quote plain scalars — Midea expects "1010", "stamp", etc. as strings
        return '"' . json_escape($v) . '"';
    }
}

sub json_get {
    my ( $json, $key ) = @_;
    return $1
      if $json =~ /"$key"\s*:\s*"((?:\\.|[^"\\])*)"/;
    return $1
      if $json =~ /"$key"\s*:\s*(-?[0-9]+(?:\.[0-9]+)?)/;
    return $1
      if $json =~ /"$key"\s*:\s*(true|false|null)/;
    return undef;
}

sub json_code {
    my ($json) = @_;
    my $c = json_get( $json, 'code' );
    $c = json_get( $json, 'errorCode' ) unless defined $c;
    return defined $c ? 0 + $c : -1;
}

sub api_data {
    my ( $alias, $payload ) = @_;
    my $raw  = https_json( $alias, $payload );
    my $code = json_code($raw);
    return ( $code, $raw );
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

# --- cloud login ---

sub probe_login_id_host {
    my ($account) = @_;
    my $g = general_data();
    $g->{loginAccount} = $account;
    my ( $code, $raw ) = api_data( '/v1/user/login/id/get', $g );
    return ( $code == 0 ) ? ( json_get( $raw, 'loginId' ) // '' ) : '';
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
        my $lid = probe_login_id_host($account);
        if ( length $lid ) {
            print "loginId found\n";
            return;
        }
        print "no\n";
    }
    die "Account not found on any regional MAS (tried: @MAS_HOSTS)\n";
}

sub cloud_login {
    my ( $account, $password, %opt ) = @_;
    $DEVICE_ID = cloud_device_id($account);
    resolve_region( $account, $opt{region} );

    # Optional route (ignore failures — overseas often returns 10004)
    {
        my $g = general_data();
        $g->{userType} = '0';
        $g->{userName} = $account;
        api_data( '/v1/multicloud/platform/user/route', $g );
    }

    my $g = general_data();
    $g->{loginAccount} = $account;
    my ( $c1, $r1 ) = api_data( '/v1/user/login/id/get', $g );
    die "login/id/get failed (code=$c1): $r1\n" if $c1 != 0;
    my $login_id = json_get( $r1, 'loginId' )
      // die "no loginId in: $r1\n";

    my $iot = general_data();
    delete $iot->{uid};
    $iot->{loginAccount} = $account;
    $iot->{password}     = encrypt_password( $login_id, $password );
    $iot->{iampwd}       = encrypt_iam_password( $login_id, $password );
    $iot->{stamp}        = stamp();

    my $body = {
        iotData => $iot,
        data    => {
            appKey   => APP_KEY,
            deviceId => $DEVICE_ID,
            platform => '2',
        },
        stamp => $iot->{stamp},
    };

    my ( $c2, $r2 ) = api_data( '/mj/user/login', $body );
    die "login failed (code=$c2): $r2\n" if $c2 != 0;

    $UID = json_get( $r2, 'uid' ) // '';

    # HTTP header token is the short mdata.accessToken (midealan SmartHomeCloud).
    # Long data.accessToken + randomData are AES session material for SN decrypt.
    if ( $r2 =~ /"mdata"\s*:\s*\{.*?"accessToken"\s*:\s*"([^"]+)"/s ) {
        $ACCESS_TOKEN = $1;
    }
    elsif ( $r2 =~ /"accessToken"\s*:\s*"(T1[^"]+)"/ ) {
        $ACCESS_TOKEN = $1;
    }
    else {
        die "no mdata.accessToken in login response: $r2\n";
    }
    if ( $r2 =~ /"randomData"\s*:\s*"([^"]+)"/ ) {
        $AES_IV = $1;
    }
    # Prefer the long hex accessToken (outside mdata) for AES
    while ( $r2 =~ /"accessToken"\s*:\s*"([^"]+)"/g ) {
        my $t = $1;
        $AES_TOKEN = $t if length($t) >= 32 && $t =~ /^[0-9a-fA-F]+$/;
    }

    dbg( 'uid',   $UID );
    dbg( 'token', substr( $ACCESS_TOKEN, 0, 12 ) . '...' );
    derive_session_aes();
    $TX_DEVICE_ID = req_id() . req_id();    # 32 hex like the app
    $TX_DEVICE_ID = substr( $TX_DEVICE_ID, 0, 32 );
    fetch_home_id();
    return 1;
}

sub fetch_home_id {
    my $g = general_data();
    my ( $code, $raw ) = api_data( '/v1/homegroup/list/get', $g );
    if ( $code == 0 && $raw =~ /"homegroupId"\s*:\s*"([^"]+)"/ ) {
        $HOME_ID = $1;
        dbg( 'homeId', $HOME_ID );
    }
    else {
        dbg( 'homeId', "fetch failed code=$code" );
    }
}

# SN / app2base payloads: AES-128-CBC with session dataKey/dataIv (16 ASCII chars).
sub decrypt_sn {
    my ($hex) = @_;
    return '' unless defined $hex && length($hex) >= 32;
    return $hex unless $hex =~ /^[0-9a-fA-F]+$/;
    return $hex unless length($DATA_KEY) >= 16 && length($DATA_IV) >= 16;

    my $p = eval { aes128_cbc( 0, $DATA_KEY, $DATA_IV, pack( 'H*', $hex ) ) };
    return $hex if $@ || !defined $p;
    $p =~ s/[\x00-\x1f]+$//;
    return ( $p =~ /^[\x20-\x7e]{8,}$/ ) ? $p : $hex;
}

sub list_appliances {
    my $g = general_data();
    my ( $code, $raw ) = api_data( '/v1/appliance/user/list/get', $g );
    die "list appliances failed (code=$code): $raw\n" if $code != 0;

    my @rows;
    # Naive scan for appliance objects
    while ( $raw =~ /\{([^{}]*"id"\s*:\s*"?\d+"?[^{}]*)\}/g ) {
        my $chunk = $1;
        my $id    = json_get( "{$chunk}", 'id' );
        next unless defined $id;
        my $name = json_get( "{$chunk}", 'name' ) // '';
        my $type = json_get( "{$chunk}", 'type' ) // '';
        my $sn   = json_get( "{$chunk}", 'sn' )   // '';
        my $on   = json_get( "{$chunk}", 'onlineStatus' ) // '';
        push @rows,
          {
            id     => $id,
            name   => $name,
            type   => $type,
            sn     => decrypt_sn($sn),
            online => ( $on eq '1' || $on eq 1 ) ? 'yes' : 'no',
          };
    }
    return \@rows;
}

# --- iCheck via /v1/app2base/data/transmit ---

sub app2base_transmit {
    my ( $service_url, $payload ) = @_;
    die "session AES keys missing (login first)\n"
      unless length($DATA_KEY) >= 16 && length($DATA_IV) >= 16;

    my $plain = json_encode($payload);
    my $cipher_hex =
      unpack( 'H*', aes128_cbc( 1, $DATA_KEY, $DATA_IV, $plain ) );

    my $body = {
        proType         => '0xAC',
        appVersion      => '3.18.0',
        data            => $cipher_hex,
        src             => '10',
        retryCount      => '3',
        format          => \('2'),
        androidApiLevel => '29',
        stamp           => stamp(),
        language        => 'en',
        clientVersion   => '3.18.0',
        ( length($HOME_ID) ? ( homegroupId => $HOME_ID ) : () ),
        deviceId        => ( length($TX_DEVICE_ID) ? $TX_DEVICE_ID : ( $DEVICE_ID . $DEVICE_ID ) ),
        reqId           => req_id(),
        uid             => $UID,
        clientType      => \('1'),
        param           => { serviceUrl => $service_url },
        appId           => APP_ID,
        appVNum         => '3.18.0',
        deviceBrand     => 'ac-check.pl',
    };

    my $raw = https_json(
        '/v1/app2base/data/transmit',
        $body,
        random => millis_random(),
    );
    return ( json_code($raw), $raw );
}

sub icheck_payload {
    my ( $appliance_id, %extra ) = @_;
    # Plugin caches machine_type (often 20000) as applianceType for iCheck.
    # Using "0xAC" yields empty result{}; "20000" returns historyId=1 then real id.
    return {
        appId         => APP_ID,
        userId        => $UID,
        applianceId   => "$appliance_id",
        lang          => 'en',
        applianceType => '20000',
        %extra,
    };
}

sub check_start {
    my ($appliance_id) = @_;
    # Plugin: orderId = applianceId + YYYYMMDDHHmmss (UTC)
    my $order_id = $appliance_id . stamp();
    my ( $code, $raw ) = app2base_transmit(
        '/orac-check/icheck/check/hand',
        icheck_payload( $appliance_id, orderId => $order_id ),
    );
    return ( $code, $raw, $order_id );
}

# Extract iCheck business payload fields from MAS transmit response.
# App path: datas = JSON.parse(data.data); historyId = datas.result.historyId
# Overseas MAS often returns data already as an object (not a string).
sub parse_hand_result {
    my ($raw) = @_;
    my $err = json_get( $raw, 'errCode' );
    my $history_id;

    # Prefer result.historyId (string or number)
    if ( $raw =~ /"result"\s*:\s*\{([^{}]*)\}/s ) {
        my $result = '{' . $1 . '}';
        $history_id = json_get( $result, 'historyId' );
    }
    $history_id = json_get( $raw, 'historyId' )
      if !defined $history_id || $history_id eq '';

    return ( $err, $history_id );
}

# Plugin semantics for check/hand:
#   historyId == orderId  → ready; use it for checkSummary
#   historyId == 1        → still running; re-POST hand with same orderId
#   historyId == 2        → failed
#   historyId == 4        → cancelled / give up
sub wait_for_history_id {
    my ( $appliance_id, $order_id, $raw, $max_polls, $interval ) = @_;
    $max_polls //= 12;
    $interval  //= 5;

    my ( $err, $hid ) = parse_hand_result($raw);
    dbg( 'hand_err',  $err // '' );
    dbg( 'hand_hid',  defined $hid ? $hid : '(none)' );

    for my $i ( 0 .. $max_polls ) {
        if ( defined $err && $err ne '' && $err ne '0' && $err != 0 ) {
            return ( undef, $raw, "errCode=$err" );
        }

        if ( defined $hid && length($hid) ) {
            return ( undef, $raw, "failed (historyId=2)" ) if $hid eq '2' || $hid == 2;
            return ( undef, $raw, "cancelled (historyId=4)" )
              if $hid eq '4' || $hid == 4;

            # Ready: real id (usually equals orderId once device finishes)
            if ( $hid eq $order_id || ( length($hid) > 8 && $hid ne '1' ) ) {
                return ( $hid, $raw, undef );
            }

            # historyId==1 → still checking; fall through to poll
        }

        last if $i == $max_polls;
        print "  polling check/hand ($i/" . ($max_polls) . "), historyId="
          . ( defined $hid ? $hid : 'null' ) . "...\n";
        sleep $interval;
        my ( $code, $next ) = app2base_transmit(
            '/orac-check/icheck/check/hand',
            icheck_payload( $appliance_id, orderId => $order_id ),
        );
        return ( undef, $next, "poll failed code=$code" ) if $code != 0;
        $raw = $next;
        ( $err, $hid ) = parse_hand_result($raw);
    }

    return ( undef, $raw, 'timeout waiting for historyId' );
}

sub check_details {
    my ( $appliance_id, $history_id ) = @_;
    my %extra;
    $extra{historyId} = $history_id if defined $history_id && length $history_id;

    # Historical runs: faults live in history/detail (status=1).
    # Live/just-finished: checkSummary has totalErrorCount + faultList.
    if ( defined $history_id && length $history_id ) {
        my ( $code, $raw ) = app2base_transmit(
            '/orac-check/icheck/history/detail',
            icheck_payload( $appliance_id, %extra ),
        );
        return ( $code, $raw, '/orac-check/icheck/history/detail' ) if $code == 0;
    }
    my ( $code, $raw ) = app2base_transmit(
        '/orac-check/icheck/checkSummary',
        icheck_payload( $appliance_id, %extra ),
    );
    return ( $code, $raw, '/orac-check/icheck/checkSummary' );
}

# Plugin moreDetails page: checkMore → result.faultList + result.normalList
sub check_more {
    my ( $appliance_id, $history_id ) = @_;
    die "checkMore needs --history-id (from check/hand or --history)\n"
      unless defined $history_id && length $history_id;
    return app2base_transmit(
        '/orac-check/icheck/checkMore',
        icheck_payload( $appliance_id, historyId => $history_id ),
    );
}

sub check_history {
    my ($appliance_id) = @_;
    return app2base_transmit(
        '/orac-check/icheck/history/list',
        icheck_payload(
            $appliance_id,
            page     => \('1'),
            pageSize => \('10'),
        ),
    );
}

sub check_summary {
    my ( $appliance_id, $history_id ) = @_;
    my %extra;
    $extra{historyId} = $history_id if defined $history_id && length $history_id;
    return app2base_transmit(
        '/orac-check/icheck/checkSummary',
        icheck_payload( $appliance_id, %extra ),
    );
}

sub print_check_items {
    my ($json) = @_;
    my $found = 0;

    my $summary = json_get( $json, 'checkDetailSummary' );
    print "$summary\n" if defined $summary && length $summary;

    my $total = json_get( $json, 'totalItemCount' );
    my $errs  = json_get( $json, 'totalErrorCount' );
    if ( defined $total || defined $errs ) {
        printf "items=%s  errors=%s\n", $total // '?', $errs // '?';
        $found++;
    }

    my $question = json_get( $json, 'question' );
    print "Q: $question\n" if defined $question && length $question;

    # history/detail faultLibList entries (status 1 = error)
    while ( $json =~ /\{([^{}]*"ecode"[^{}]*)\}/g ) {
        my $item = '{' . $1 . '}';
        my $status = json_get( $item, 'status' );
        next unless defined $status && ( $status eq '1' || $status == 1 );
        my $desc = json_get( $item, 'description' ) // '?';
        my $ecode = json_get( $item, 'ecode' ) // '?';
        my $code  = json_get( $item, 'showCode' ) // '';
        my $tip   = json_get( $item, 'levelTip' ) // 'Error';
        my $sug   = json_get( $item, 'suggestion' ) // '';
        $sug = '' if $sug =~ /^nul/i || $sug eq 'null';
        printf "FAULT  %s  %s  [%s/%s]\n", $ecode, $desc, $tip, $code;
        print "       suggestion: $sug\n" if length $sug;
        $found++;
    }

    if ( $json =~ /"answerList"\s*:\s*\[(.*?)\]/s ) {
        my $arr = $1;
        while ( $arr =~ /"((?:\\.|[^"\\])*)"/g ) {
            print "TIP    $1\n";
            $found++;
        }
    }

    # faultList entries (objects with itemCode, or plain strings in checkMore)
    if ( $json =~ /"faultList"\s*:\s*\[(.*?)\]/s ) {
        my $arr = $1;
        if ( $arr =~ /\{/ ) {
            while ( $arr =~ /\{([^{}]*)\}/g ) {
                my $item   = '{' . $1 . '}';
                my $name   = json_get( $item, 'itemName' ) // json_get( $item, 'name' ) // '?';
                my $result = json_get( $item, 'result' ) // json_get( $item, 'status' ) // '?';
                my $icode  = json_get( $item, 'itemCode' ) // '?';
                printf "FAULT  %-12s %-32s %s\n", $icode, $name, $result;
                $found++;
            }
        }
        else {
            while ( $arr =~ /"((?:\\.|[^"\\])*)"/g ) {
                print "FAULT  $1\n";
                $found++;
            }
        }
    }

    if ( $json =~ /"normalList"\s*:\s*\[(.*?)\]/s ) {
        my $arr = $1;
        my $n   = 0;
        while ( $arr =~ /"((?:\\.|[^"\\])*)"/g ) {
            print "OK     $1\n";
            $found++;
            $n++;
        }
        print "(normalList: $n items)\n" if $n;
    }

    while ( $json =~ /\{([^{}]*"itemCode"[^{}]*)\}/g ) {
        my $item   = '{' . $1 . '}';
        my $name   = json_get( $item, 'itemName' ) // json_get( $item, 'name' ) // '?';
        my $result = json_get( $item, 'result' ) // json_get( $item, 'status' ) // '?';
        my $icode  = json_get( $item, 'itemCode' ) // '?';
        printf "%-32s %-12s %s\n", $name, $icode, $result;
        $found++;
    }

    # history list rows
    while ( $json =~ /\{([^{}]*"historyId"[^{}]*)\}/g ) {
        my $item = '{' . $1 . '}';
        my $hid  = json_get( $item, 'historyId' ) // '?';
        my $desc = json_get( $item, 'checkResultDesc' ) // '';
        my $when = json_get( $item, 'checkTime' ) // '';
        my $en   = json_get( $item, 'errorNum' );
        printf "%s  %s  %s%s\n", $when, $hid, $desc,
          ( defined $en ? " (errors=$en)" : '' );
        $found++;
    }
    print $json, "\n" unless $found;
}

# --- CLI ---

my %opt;
Getopt::Long::GetOptions(
    \%opt,
    'user=s',
    'pass=s',
    'device-id=s',
    'history-id=s',
    'list',
    'start',
    'details',
    'more',
    'history',
    'summary',
    'region=s',
    'poll=i',
    'debug',
    'help',
) or die_usage(2);

die_usage(0) if $opt{help};

$opt{user} //= $ENV{MIDEA_EMAIL};
$opt{pass} //= $ENV{MIDEA_PASSWORD};

die_usage(2) unless $opt{user} && $opt{pass};

$DEBUG = 1 if $opt{debug};
my $poll = $opt{poll} // 15;

unless ( $opt{list} || $opt{start} || $opt{details} || $opt{more}
    || $opt{history} || $opt{summary} )
{
    # default: list if no device; else start+details
    if ( $opt{'device-id'} ) {
        $opt{start} = 1;
    }
    else {
        $opt{list} = 1;
    }
}

print "Authenticating as $opt{user}...\n";
eval {
    cloud_login( $opt{user}, $opt{pass}, region => $opt{region} );
    1;
} or do {
    print STDERR "Login failed: $@";
    print STDERR <<"HINT";

Hints:
  - Use the same email/password as in the MSmartLife / SmartHome app
    that bound this AC (code 3102 = wrong password *or* wrong regional MAS).
  - Omit --region to auto-probe MAS hosts, or try: --region us
  - Pass credentials via env to avoid shell history:
      export MIDEA_EMAIL='you\@example.com'
      export MIDEA_PASSWORD='...'
HINT
    exit 1;
};
print "OK (uid=$UID)\n";

if ( $opt{list} ) {
    print "Appliances:\n";
    my $rows = list_appliances();
    if (@$rows) {
        printf "%-18s %-8s %-6s %s\n", 'id', 'type', 'online', 'name / sn';
        printf "%s\n", '-' x 72;
        for my $r (@$rows) {
            printf "%-18s %-8s %-6s %s  %s\n",
              $r->{id}, $r->{type}, $r->{online}, $r->{name}, $r->{sn};
        }
    }
    else {
        print "(none parsed — raw response)\n";
        my $g = general_data();
        my ( undef, $raw ) = api_data( '/v1/appliance/user/list/get', $g );
        print $raw, "\n";
    }
}

my $aid = $opt{'device-id'};
my $history_id = $opt{'history-id'};

if ( $aid && $opt{start} ) {
    print "Starting iCheck for appliance $aid (via app2base transmit)...\n";
    my ( $code, $raw, $order_id ) = check_start($aid);
    if ( $code != 0 ) {
        print STDERR "start failed (code=$code): $raw\n";
        if ( $code == 4010 ) {
            print STDERR "DECRYPT_ERROR: session AES key derivation failed.\n";
        }
        exit 1;
    }
    print "Accepted orderId=$order_id\n";
    print "$raw\n";

    # historyId comes in check/hand response (datas.result.historyId).
    # 1 = still running → re-call hand with same orderId (app polls every 5s).
    my $polls = int( ( $poll // 15 ) / 5 );
    $polls = 4 if $polls < 4;
    my ( $hid, $last, $fail ) =
      wait_for_history_id( $aid, $order_id, $raw, $polls, 5 );
    if ($fail) {
        print STDERR "iCheck did not yield historyId: $fail\n";
        print STDERR "$last\n" if defined $last;
        exit 1;
    }
    $history_id = $hid;
    print "Ready historyId=$history_id\n";
    $opt{details} = 1;
}

if ( $aid && $opt{details} ) {
    print "Fetching details...\n";
    my ( $code, $raw, $url ) = check_details( $aid, $history_id );
    die "details failed via $url (code=$code): $raw\n" if $code != 0;
    print "--- $url ---\n";
    print_check_items($raw);
}

if ( $aid && $opt{more} ) {
    print "Fetching checkMore...\n";
    my ( $code, $raw ) = check_more( $aid, $history_id );
    die "checkMore failed (code=$code): $raw\n" if $code != 0;
    print "--- /orac-check/icheck/checkMore ---\n";
    print_check_items($raw);
}

if ( $aid && $opt{history} ) {
    my ( $code, $raw ) = check_history($aid);
    die "history failed (code=$code): $raw\n" if $code != 0;
    print_check_items($raw);
}

if ( $aid && $opt{summary} ) {
    my ( $code, $raw ) = check_summary( $aid, $history_id );
    die "summary failed (code=$code): $raw\n" if $code != 0;
    print_check_items($raw);
}

exit 0;
