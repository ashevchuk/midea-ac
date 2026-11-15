#!/usr/bin/env perl
#
# ac-ng.pl — LAN control driven by a generated protocol map
# (reverse/lua/<model>.protocol.json, from reverse/lua/lua2map.pl) instead
# of a hand-ported byte-offset table. Pure Perl, standard library only —
# no CPAN modules, no lua/luajit at runtime (that's generator-only).
#
# Network/crypto layer (packet framing, AES-128-ECB, KEY_SIGN) is the same
# approach as ac.pl, duplicated here per this project's convention of
# self-contained scripts.
#
use strict;
use warnings;

use Getopt::Long ();
use POSIX        ();
use List::Util   ();
use Socket       ();
use IO::Socket   ();
use IO::Handle   ();

use constant {
    PORT         => 6444,
    TIMEOUT      => 4,
    RETRY        => 8,
    BLOCK_LEN    => 16,
    RESPONSE_LEN => 256,
    EMPTY_STR    => '',
};

use constant { KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S' };
use constant { KEY => '6a92ef406bad2f0359baad994171ea6d' }; # md5(KEY_SIGN)

use constant {
    PACKET => [
        0x5a, 0x5a, 0x01, 0x11, 0x68, 0x00, 0x20, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ],
    # Generic status query (type 0x41) — not property-specific, same for
    # every model, so this is not part of the generated map.
    STATUS_QUERY => [
        0xaa, 0x20, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x41, 0x81, 0x00, 0xff, 0x03, 0xff,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ],
};

our $DEBUG = 0;

sub dbg {
    return unless $DEBUG;
    my ( $label, $data ) = @_;
    printf STDERR "[DEBUG] %-10s %s\n", $label,
      ref($data) eq 'ARRAY' ? ahex($data) : $data;
}

sub ahex {
    my ($item) = @_;
    return ref($item) eq 'ARRAY'
      ? join( EMPTY_STR, map { sprintf( '%.2x', $_ ) } @{$item} )
      : unpack( 'H*', $item );
}

# --- Pure Perl AES-128-ECB (verbatim from ac.pl) ---
{
    package PureAES;
    use strict;
    use warnings;

    my @S = (
        0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
        0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
        0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
        0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
        0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
        0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
        0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
        0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
        0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
        0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
        0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
        0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
        0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
        0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
        0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
        0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16,
    );
    my @Si = (
        0x52,0x09,0x6a,0xd5,0x30,0x36,0xa5,0x38,0xbf,0x40,0xa3,0x9e,0x81,0xf3,0xd7,0xfb,
        0x7c,0xe3,0x39,0x82,0x9b,0x2f,0xff,0x87,0x34,0x8e,0x43,0x44,0xc4,0xde,0xe9,0xcb,
        0x54,0x7b,0x94,0x32,0xa6,0xc2,0x23,0x3d,0xee,0x4c,0x95,0x0b,0x42,0xfa,0xc3,0x4e,
        0x08,0x2e,0xa1,0x66,0x28,0xd9,0x24,0xb2,0x76,0x5b,0xa2,0x49,0x6d,0x8b,0xd1,0x25,
        0x72,0xf8,0xf6,0x64,0x86,0x68,0x98,0x16,0xd4,0xa4,0x5c,0xcc,0x5d,0x65,0xb6,0x92,
        0x6c,0x70,0x48,0x50,0xfd,0xed,0xb9,0xda,0x5e,0x15,0x46,0x57,0xa7,0x8d,0x9d,0x84,
        0x90,0xd8,0xab,0x00,0x8c,0xbc,0xd3,0x0a,0xf7,0xe4,0x58,0x05,0xb8,0xb3,0x45,0x06,
        0xd0,0x2c,0x1e,0x8f,0xca,0x3f,0x0f,0x02,0xc1,0xaf,0xbd,0x03,0x01,0x13,0x8a,0x6b,
        0x3a,0x91,0x11,0x41,0x4f,0x67,0xdc,0xea,0x97,0xf2,0xcf,0xce,0xf0,0xb4,0xe6,0x73,
        0x96,0xac,0x74,0x22,0xe7,0xad,0x35,0x85,0xe2,0xf9,0x37,0xe8,0x1c,0x75,0xdf,0x6e,
        0x47,0xf1,0x1a,0x71,0x1d,0x29,0xc5,0x89,0x6f,0xb7,0x62,0x0e,0xaa,0x18,0xbe,0x1b,
        0xfc,0x56,0x3e,0x4b,0xc6,0xd2,0x79,0x20,0x9a,0xdb,0xc0,0xfe,0x78,0xcd,0x5a,0xf4,
        0x1f,0xdd,0xa8,0x33,0x88,0x07,0xc7,0x31,0xb1,0x12,0x10,0x59,0x27,0x80,0xec,0x5f,
        0x60,0x51,0x7f,0xa9,0x19,0xb5,0x4a,0x0d,0x2d,0xe5,0x7a,0x9f,0x93,0xc9,0x9c,0xef,
        0xa0,0xe0,0x3b,0x4d,0xae,0x2a,0xf5,0xb0,0xc8,0xeb,0xbb,0x3c,0x83,0x53,0x99,0x61,
        0x17,0x2b,0x04,0x7e,0xba,0x77,0xd6,0x26,0xe1,0x69,0x14,0x63,0x55,0x21,0x0c,0x7d,
    );
    my @RCON = ( 0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1b,0x36 );

    sub _mul {
        my ( $a, $b ) = @_;
        my $r = 0;
        for ( 1 .. 8 ) {
            $r ^= $a if $b & 1;
            my $h = $a & 0x80;
            $a = ( $a << 1 ) & 0xff;
            $a ^= 0x1b if $h;
            $b >>= 1;
        }
        return $r;
    }

    sub _key_expand {
        my @k = unpack( 'C*', $_[0] );
        my @w;
        $w[$_] = ( $k[$_*4] << 24 ) | ( $k[$_*4+1] << 16 ) | ( $k[$_*4+2] << 8 ) | $k[$_*4+3]
            for 0 .. 3;
        for my $i ( 4 .. 43 ) {
            my $t = $w[$i-1];
            if ( $i % 4 == 0 ) {
                $t = ( ( $t << 8 ) | ( $t >> 24 ) ) & 0xffffffff;
                $t = ( $S[($t>>24)&0xff] << 24 ) | ( $S[($t>>16)&0xff] << 16 )
                   | ( $S[($t>> 8)&0xff] <<  8 ) |   $S[ $t      &0xff];
                $t ^= $RCON[$i/4-1] << 24;
            }
            $w[$i] = ( $w[$i-4] ^ $t ) & 0xffffffff;
        }
        return \@w;
    }

    sub _add_rk {
        my ( $s, $w, $rnd ) = @_;
        for my $c ( 0 .. 3 ) {
            my $wrd = $w->[$rnd*4+$c];
            $s->[$c*4]   ^= ( $wrd >> 24 ) & 0xff;
            $s->[$c*4+1] ^= ( $wrd >> 16 ) & 0xff;
            $s->[$c*4+2] ^= ( $wrd >>  8 ) & 0xff;
            $s->[$c*4+3] ^=   $wrd         & 0xff;
        }
    }

    sub _enc_block {
        my ( $in, $w ) = @_;
        my @s = @$in;
        _add_rk( \@s, $w, 0 );
        for my $rnd ( 1 .. 9 ) {
            $s[$_] = $S[$s[$_]] for 0 .. 15;
            @s[1,5,9,13]  = @s[5,9,13,1];
            @s[2,6,10,14] = @s[10,14,2,6];
            @s[3,7,11,15] = @s[15,3,7,11];
            for my $c ( 0 .. 3 ) {
                my ( $a, $b, $cc, $d ) = @s[$c*4 .. $c*4+3];
                $s[$c*4]   = _mul(2,$a) ^ _mul(3,$b) ^       $cc  ^       $d;
                $s[$c*4+1] =      $a   ^ _mul(2,$b)  ^ _mul(3,$cc) ^       $d;
                $s[$c*4+2] =      $a   ^       $b    ^ _mul(2,$cc) ^ _mul(3,$d);
                $s[$c*4+3] = _mul(3,$a) ^      $b   ^       $cc   ^ _mul(2,$d);
            }
            _add_rk( \@s, $w, $rnd );
        }
        $s[$_] = $S[$s[$_]] for 0 .. 15;
        @s[1,5,9,13]  = @s[5,9,13,1];
        @s[2,6,10,14] = @s[10,14,2,6];
        @s[3,7,11,15] = @s[15,3,7,11];
        _add_rk( \@s, $w, 10 );
        return \@s;
    }

    sub _dec_block {
        my ( $in, $w ) = @_;
        my @s = @$in;
        _add_rk( \@s, $w, 10 );
        for my $rnd ( reverse 1 .. 9 ) {
            @s[1,5,9,13]  = @s[13,1,5,9];
            @s[2,6,10,14] = @s[10,14,2,6];
            @s[3,7,11,15] = @s[7,11,15,3];
            $s[$_] = $Si[$s[$_]] for 0 .. 15;
            _add_rk( \@s, $w, $rnd );
            for my $c ( 0 .. 3 ) {
                my ( $a, $b, $cc, $d ) = @s[$c*4 .. $c*4+3];
                $s[$c*4]   = _mul(14,$a) ^ _mul(11,$b) ^ _mul(13,$cc) ^ _mul( 9,$d);
                $s[$c*4+1] = _mul( 9,$a) ^ _mul(14,$b) ^ _mul(11,$cc) ^ _mul(13,$d);
                $s[$c*4+2] = _mul(13,$a) ^ _mul( 9,$b) ^ _mul(14,$cc) ^ _mul(11,$d);
                $s[$c*4+3] = _mul(11,$a) ^ _mul(13,$b) ^ _mul( 9,$cc) ^ _mul(14,$d);
            }
        }
        @s[1,5,9,13]  = @s[13,1,5,9];
        @s[2,6,10,14] = @s[10,14,2,6];
        @s[3,7,11,15] = @s[7,11,15,3];
        $s[$_] = $Si[$s[$_]] for 0 .. 15;
        _add_rk( \@s, $w, 0 );
        return \@s;
    }

    sub encrypt_ecb {
        my ( $data, $key ) = @_;
        my @b = unpack( 'C*', $data );
        my $w = _key_expand($key);
        my $out = '';
        $out .= pack( 'C*', @{ _enc_block( [@b[$_*16 .. $_*16+15]], $w ) } ) for 0 .. $#b / 16;
        return $out;
    }

    sub decrypt_ecb {
        my ( $data, $key ) = @_;
        my @b = unpack( 'C*', $data );
        my $w = _key_expand($key);
        my $out = '';
        $out .= pack( 'C*', @{ _dec_block( [@b[$_*16 .. $_*16+15]], $w ) } ) for 0 .. $#b / 16;
        return $out;
    }

    1;
}

sub inflate { return [ map { ord } split //, $_[0] ] }
sub deflate { return join EMPTY_STR, map { defined($_) ? chr($_) : chr(0) } @{ $_[0] } }
sub encrypt { return inflate( PureAES::encrypt_ecb( deflate( $_[0] ), pack( 'H*', KEY ) ) ) }
sub decrypt { return inflate( PureAES::decrypt_ecb( deflate( $_[0] ), pack( 'H*', KEY ) ) ) }

sub md5 {
    my ($msg) = $_[0];
    my ( $a, $b, $c, $d ) = ( 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476 );
    my @T = map { int( 4294967296 * abs( sin( $_ + 1 ) ) ) } 0 .. 63;
    my @S = ( (7,12,17,22) x 4, (5,9,14,20) x 4, (4,11,16,23) x 4, (6,10,15,21) x 4 );
    my $msg_len = length($msg);
    $msg .= pack( 'C', 0x80 );
    $msg .= pack( 'C', 0 ) while ( length($msg) % 64 != 56 );
    $msg .= pack( 'V2', $msg_len * 8, 0 );
    my $rotl = sub { my ( $x, $n ) = @_; $x &= 0xffffffff; ( ( $x << $n ) | ( $x >> ( 32 - $n ) ) ) & 0xffffffff };
    for ( my $i = 0; $i < length($msg); $i += 64 ) {
        my @M = unpack( 'V16', substr( $msg, $i, 64 ) );
        my ( $A, $B, $C, $D ) = ( $a, $b, $c, $d );
        for my $j ( 0 .. 63 ) {
            my ( $F, $g );
            if ( $j < 16 ) { $F = ( $B & $C ) | ( ( ~$B & 0xffffffff ) & $D ); $g = $j; }
            elsif ( $j < 32 ) { $F = ( $D & $B ) | ( ( ~$D & 0xffffffff ) & $C ); $g = ( 5 * $j + 1 ) % 16; }
            elsif ( $j < 48 ) { $F = $B ^ $C ^ $D; $g = ( 3 * $j + 5 ) % 16; }
            else { $F = $C ^ ( $B | ( ~$D & 0xffffffff ) ); $g = ( 7 * $j ) % 16; }
            $F = ( $F + $A + $T[$j] + $M[$g] ) & 0xffffffff;
            $A = $D; $D = $C; $C = $B; $B = ( $B + $rotl->( $F, $S[$j] ) ) & 0xffffffff;
        }
        $a = ( $a + $A ) & 0xffffffff; $b = ( $b + $B ) & 0xffffffff;
        $c = ( $c + $C ) & 0xffffffff; $d = ( $d + $D ) & 0xffffffff;
    }
    return pack( 'V4', $a, $b, $c, $d );
}

use constant {
    CRC8_TABLE_GEN => sub {
        my ( $p, $v ) = ( 0x00, $_[1] );
        do { $p |= 0x01 << ( $_[0] - $_ ) if $v & 0x01; $v = $v >> 0x01; }
          for 0x01 .. $_[0];
        return map {
            my $i = $_;
            $i = ( $i >> 0x01 ) ^ ( $i & 0x01 && $p ) for 0x00 .. 0x07;
            $i & 0x02**$_[0] - 0x01
        } 0x00 .. 0xff;
    }
};
use constant { CRC8_TABLE => [ CRC8_TABLE_GEN->( 8, 0x0131 ) ] };

sub crc8 {
    my $crc = 0;
    $crc = CRC8_TABLE->[ $crc ^ $_ ] for @{ $_[0] };
    return $crc;
}

sub packet {
    my ($command) = @_;
    my $packet = [ @{ +PACKET } ];

    push @{$command}, ( ( ~List::Util::sum0( @{$command}[ 0x01 .. $#$command ] ) + 0x01 ) & 0xff );

    my $pad = BLOCK_LEN - ( scalar(@$command) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt($command) };
    $packet->[0x04] = scalar(@$packet) + BLOCK_LEN;
    push @{$packet}, @{ inflate md5( deflate($packet) . KEY_SIGN ) };

    return $packet;
}

sub net_request {
    my ( $device_ip, $data, $recv_len ) = @_;
    $recv_len //= RESPONSE_LEN;

    dbg( '>> TX', $data );

    my $client = IO::Socket->new(
        Domain    => IO::Socket::AF_INET,
        Type      => IO::Socket::SOCK_STREAM,
        Proto     => 'tcp',
        Timeout   => TIMEOUT,
        ReusePort => 1,
        PeerPort  => PORT,
        PeerHost  => $device_ip,
    ) or die "Socket error: $@";

    $client->send( deflate $data ) == scalar @{$data}
      or $client->close(), die "Send error";

    my $buffer = '';
    {
        local $SIG{ALRM} = sub { die "timeout" };
        alarm TIMEOUT;
        eval { $client->recv( $buffer, $recv_len ); 1 } or do {
            alarm 0;
            $client->close();
            die $@ || "timeout";
        };
        alarm 0;
    }
    $client->close();

    my $len = length($buffer);
    die "no response" unless $len;

    my $raw = inflate $buffer;
    dbg( '<< RX', $raw );

    my $enc_end = $len >= 17 ? $len - 17 : 0x27;
    $enc_end = 0x27 if $enc_end < 0x28;
    return [ @{$raw}[ 0x28 .. $enc_end ] ];
}

sub send_request {
    my ( $device_ip, $data, $recv_len ) = @_;
    my $response = decrypt( net_request( $device_ip, $data, $recv_len ) );
    my $unpadded = [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
    dbg( '<< RX/cmd', $unpadded );
    return $unpadded;
}

sub request_with_retry {
    my ( $device_ip, $packet, $validate ) = @_;
    my $last_error;
    my $retry = RETRY;
    while ( --$retry ) {
        my $data = eval {
            my $d = send_request( $device_ip, $packet );
            die "unexpected response type\n" if $validate && !$validate->($d);
            $d;
        };
        if ( !$data ) { $last_error = $@; next; }
        return $data;
    }
    die $last_error;
}

# --- Minimal JSON decoder (reads the generated map only; no encoder needed) ---

sub json_decode {
    my ($text) = @_;
    my $pos = 0;
    my $parse_value;

    my $skip_ws = sub { $pos++ while substr( $text, $pos, 1 ) =~ /^\s$/ };

    my $parse_string = sub {
        $pos++; # opening quote
        my $out = '';
        while (1) {
            my $c = substr( $text, $pos, 1 );
            die "unterminated string" if $c eq '';
            last if $c eq '"';
            if ( $c eq '\\' ) {
                my $e = substr( $text, $pos + 1, 1 );
                my %map = ( n => "\n", r => "\r", t => "\t", '"' => '"', '\\' => '\\', '/' => '/' );
                if ( exists $map{$e} ) { $out .= $map{$e}; $pos += 2; }
                elsif ( $e eq 'u' ) { $out .= chr( hex( substr( $text, $pos + 2, 4 ) ) % 256 ); $pos += 6; }
                else { $out .= $e; $pos += 2; }
            }
            else { $out .= $c; $pos++; }
        }
        $pos++; # closing quote
        return $out;
    };

    $parse_value = sub {
        $skip_ws->();
        my $c = substr( $text, $pos, 1 );
        if ( $c eq '"' ) { return $parse_string->(); }
        if ( $c eq '{' ) {
            $pos++; $skip_ws->();
            my %obj;
            if ( substr( $text, $pos, 1 ) eq '}' ) { $pos++; return \%obj; }
            while (1) {
                $skip_ws->();
                my $k = $parse_string->();
                $skip_ws->();
                $pos++; # colon
                $obj{$k} = $parse_value->();
                $skip_ws->();
                my $d = substr( $text, $pos, 1 );
                $pos++;
                last if $d eq '}';
                die "expected , or } " unless $d eq ',';
            }
            return \%obj;
        }
        if ( $c eq '[' ) {
            $pos++; $skip_ws->();
            my @arr;
            if ( substr( $text, $pos, 1 ) eq ']' ) { $pos++; return \@arr; }
            while (1) {
                push @arr, $parse_value->();
                $skip_ws->();
                my $d = substr( $text, $pos, 1 );
                $pos++;
                last if $d eq ']';
                die "expected , or ] " unless $d eq ',';
            }
            return \@arr;
        }
        if ( substr( $text, $pos, 4 ) eq 'true' )  { $pos += 4; return 1; }
        if ( substr( $text, $pos, 5 ) eq 'false' ) { $pos += 5; return 0; }
        if ( substr( $text, $pos, 4 ) eq 'null' )  { $pos += 4; return undef; }
        my ($num) = ( substr( $text, $pos ) =~ /^(-?\d+\.?\d*(?:[eE][+-]?\d+)?)/ );
        die "bad JSON at pos $pos" unless defined $num;
        $pos += length($num);
        return $num + 0;
    };

    return $parse_value->();
}

# --- Protocol map: load + reduce into something the engine can drive ---

sub load_map {
    my ($path) = @_;
    open my $fh, '<', $path or die "open $path: $!\n";
    my $text = do { local $/; <$fh> };
    close $fh;
    return json_decode($text);
}

# Reduce get_results (per-bit observations) into one {byte,mask,shift} per
# property, keyed on its lowest touched byte offset (see build_get_props'
# body comment for why that, and not "most bits", is the real one).
sub build_get_props {
    my ($map) = @_;
    my %by_prop;
    for my $r ( @{ $map->{get_results} } ) {
        next if $r->{structural};
        push @{ $by_prop{ $r->{property} } }, $r;
    }
    my %props;
    for my $prop ( keys %by_prop ) {
        my %by_byte;
        push @{ $by_byte{ $_->{byte} } }, $_->{bit} for @{ $by_prop{$prop} };
        # The real encoding is reliably the lowest byte offset touched —
        # side effects of a probe (e.g. a sleep-curve table appearing
        # further into the body) always land at a higher offset, and can
        # outnumber the real bits (seen for "temperature": 5 artifact bits
        # at byte 13 vs its real 4 bits at byte 2).
        my ($primary_byte) = sort { $a <=> $b } keys %by_byte;
        my @bits = sort { $a <=> $b } @{ $by_byte{$primary_byte} };
        my $mask = 0;
        $mask |= ( 1 << $_ ) for @bits;
        $props{$prop} = { byte => $primary_byte + 0, mask => $mask, shift => $bits[0] };
    }
    return \%props;
}

# Reduce set_results into {byte,values} (regular) or {tag,values} (B0),
# keeping only fields whose observations landed on exactly one offset —
# more than one means the probe had a side effect (e.g. a sleep-curve
# table appearing), which is not a clean single-property encoding.
sub build_set_props {
    my ($map) = @_;
    my %props;
    for my $r ( @{ $map->{set_results} } ) {
        next unless $r->{state} eq 'derived';
        if ( $r->{kind} eq 'regular' ) {
            my %offsets;
            for my $obs ( @{ $r->{observations} } ) {
                $offsets{ $_->{offset} } = 1 for @{ $obs->{diff} };
            }
            next unless scalar( keys %offsets ) == 1;
            my ($byte) = keys %offsets;
            my %values;
            for my $obs ( @{ $r->{observations} } ) {
                my ($d) = grep { $_->{offset} == $byte } @{ $obs->{diff} };
                $values{ $obs->{probe} } = $d->{b} if $d;
            }
            $props{ $r->{field} } = { kind => 'regular', byte => $byte + 0, values => \%values };
        }
        elsif ( $r->{kind} eq 'extended_property' ) {
            next unless @{ $r->{tags} } == 1;
            my $t = $r->{tags}[0];
            $props{ $r->{field} } = { kind => 'extended_property', tag => $t->{tag} + 0, values => $t->{values} };
        }
    }
    return \%props;
}

# --- Device I/O ---

sub get_status {
    my ($ip) = @_;
    my $data = [ @{ +STATUS_QUERY } ];
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    my $body = request_with_retry(
        $ip, packet($data),
        sub { defined $_[0]->[0x0a] && ( $_[0]->[0x0a] == 0xc0 || $_[0]->[0x0a] == 0xc1 ) }
    );
    return [ @{$body}[ 0x0a .. $#$body ] ]; # body[0] = response type byte
}

sub decode_properties {
    my ( $body, $get_props ) = @_;
    my %out;
    for my $prop ( keys %$get_props ) {
        my $p = $get_props->{$prop};
        next unless defined $body->[ $p->{byte} ];
        $out{$prop} = ( $body->[ $p->{byte} ] & $p->{mask} ) >> $p->{shift};
    }
    return \%out;
}

# Known model-specific enums (not derivable from the generic byte/bit
# sweep, so hand-ported from this model's own keyB constants — the same
# "bootstrap by hand once" exception as BASE_CONTROL in the generator).
my %MODE_NAME = ( 1 => 'auto', 2 => 'cool', 3 => 'dry', 4 => 'heat', 5 => 'fan' );
my %MODE_FROM_NAME = reverse %MODE_NAME;
my %FAN_NAME = ( 102 => 'auto', 80 => 'high', 60 => 'medium', 40 => 'low', 20 => 'silent' );

# Confirmed by decrypting real TX/ACK pairs against this unit (see
# .notes/): eco is set on bit7 of its byte but read back on bit4. No other
# base-boolean field has shown this asymmetry.
my %SET_BIT_OVERRIDE = ( eco => { mask => 0x80, shift => 7 } );

# UNRELIABLE_SET: fields the generic single-byte engine can't handle yet.
# tubro used to be here too — it looked like it needed a second,
# unexplained byte, but that was the script's math.random() message-id
# byte plus its dependent checksums bleeding into the probe diff (fixed in
# harness.lua's regular_diff). pmv is genuinely two bytes (bodyBytes[17]
# and [18]), which this engine doesn't support yet — that's a real gap,
# not noise.
my %UNRELIABLE_SET = ( pmv => 1 );

sub render_value {
    my ( $prop, $raw, $set_props, $get_props ) = @_;
    return undef unless defined $raw;
    if ( $prop eq 'mode' ) { return $MODE_NAME{$raw} // "raw_$raw"; }
    if ( $prop eq 'wind_speed' ) { return $FAN_NAME{$raw} // $raw; }
    if ( $prop eq 'temperature' ) { return $raw + 16; } # small_temperature adds the .5 separately
    if ( $prop eq 'indoor_temperature' || $prop eq 'outdoor_temperature' ) {
        return 'n/a' if $raw == 0x00 || $raw == 0xff;
        return ( $raw - 50 ) / 2;
    }
    if ( my $sp = $set_props->{$prop} ) {
        my %rev;
        for my $k ( keys %{ $sp->{values} } ) {
            my $v = $sp->{values}{$k};
            $rev{$v} = $k if $k eq 'on' || $k eq 'off';
        }
        return $rev{$raw} if exists $rev{$raw};
    }
    # No set-side on/off table (e.g. a base-control field the generator
    # never had to probe) — a single-bit field is still safe to render as
    # on/off; anything wider stays a raw number rather than guessing.
    if ( $get_props->{$prop} && $get_props->{$prop}{mask} == ( 1 << $get_props->{$prop}{shift} ) ) {
        return $raw ? 'on' : 'off';
    }
    return $raw;
}

sub cmd_get {
    my ( $ip, $get_props, $set_props ) = @_;
    my $body = get_status($ip);
    my $values = decode_properties( $body, $get_props );
    if ( exists $values->{temperature} && exists $values->{small_temperature} ) {
        $values->{temperature} += 0.5 if $values->{small_temperature};
    }
    for my $prop ( sort keys %$values ) {
        next if $prop eq 'small_temperature'; # folded into temperature above
        printf "%s:%s\n", $prop, render_value( $prop, $values->{$prop}, $set_props, $get_props );
    }
}

sub cmd_set {
    my ( $ip, $base_frame_hex, $get_props, $set_props, %want ) = @_;

    my @bytes = unpack( 'C*', pack( 'H*', $base_frame_hex ) );
    my @header = @bytes[ 0 .. 9 ];
    my @body   = @bytes[ 10 .. $#bytes - 1 ]; # drop header + trailing checksum

    my @extended_tags; # [tag, value] pairs for a B0 property-list command

    for my $prop ( keys %want ) {
        my $val = $want{$prop};

        if ( $prop eq 'mode' ) {
            my $code = $MODE_FROM_NAME{$val} // die "unknown mode: $val\n";
            my $gp = $get_props->{mode} or die "no mode mapping in map\n";
            $body[ $gp->{byte} ] = ( $body[ $gp->{byte} ] & ~$gp->{mask} ) | ( $code << $gp->{shift} );
            next;
        }
        if ( $prop eq 'temperature' ) {
            my $gp = $get_props->{temperature} or die "no temperature mapping in map\n";
            my $half = ( $val * 2 ) % 2 ? 1 : 0;
            my $whole = POSIX::floor($val) - 16;
            $body[ $gp->{byte} ] = ( $body[ $gp->{byte} ] & ~$gp->{mask} ) | ( $whole << $gp->{shift} );
            if ( my $sb = $get_props->{small_temperature} ) {
                $body[ $sb->{byte} ] = ( $body[ $sb->{byte} ] & ~$sb->{mask} ) | ( $half << $sb->{shift} );
            }
            next;
        }
        if ( $prop eq 'wind_speed' ) {
            my %from_name = reverse %FAN_NAME;
            my $code = $from_name{$val} // ( $val =~ /^\d+$/ ? $val : die "unknown wind_speed: $val\n" );
            my $gp = $get_props->{wind_speed} or die "no wind_speed mapping in map\n";
            $body[ $gp->{byte} ] = ( $body[ $gp->{byte} ] & ~$gp->{mask} ) | ( $code << $gp->{shift} );
            next;
        }

        my $sp = $set_props->{$prop};

        die "$prop: known-unreliable generic set (see UNRELIABLE_SET comment), refusing rather than sending a possibly-wrong packet\n"
          if $UNRELIABLE_SET{$prop};

        if ( !$sp && $get_props->{$prop} ) {
            # A base-control boolean (power, swing, eco, turbo-ish flags...)
            # was never in the generator's probe sweep, since it was
            # already fixed in BASE_CONTROL rather than varied. Its set
            # bit is the same as its get bit for every field we've checked
            # against the real device — except eco, which this model
            # reads back on a different bit than it's written on (see
            # SET_BIT_OVERRIDE; found by live wire testing, not guessed).
            my $gp = $get_props->{$prop};
            my ( $mask, $shift ) = ( $gp->{mask}, $gp->{shift} );
            if ( my $ov = $SET_BIT_OVERRIDE{$prop} ) {
                ( $mask, $shift ) = ( $ov->{mask}, $ov->{shift} );
            }
            die "$prop is not a single-bit field — not settable generically\n"
              unless $mask == ( 1 << $shift );
            my $bit = ( $val eq 'on' ) ? 1 : ( $val eq 'off' ) ? 0
              : die "unknown value \"$val\" for $prop (use on/off)\n";
            $body[ $gp->{byte} ] = ( $body[ $gp->{byte} ] & ~$mask ) | ( $bit << $shift );
            next;
        }
        die "property not settable (not in generated map): $prop\n" unless $sp;

        if ( $sp->{kind} eq 'regular' ) {
            my $byte_val = $sp->{values}{$val};
            die "unknown value \"$val\" for $prop\n" unless defined $byte_val;
            $body[ $sp->{byte} ] = $byte_val;
        }
        elsif ( $sp->{kind} eq 'extended_property' ) {
            my $v = $sp->{values}{$val};
            die "unknown value \"$val\" for $prop\n" unless defined $v;
            push @extended_tags, [ $sp->{tag}, $v ];
        }
    }

    my $data;
    if (@extended_tags) {
        $data = [ @header, 0xb0, scalar(@extended_tags) ];
        for my $t (@extended_tags) {
            push @{$data}, ( $t->[0] & 0xff ), ( $t->[0] >> 8 ), 0x01, ( $t->[1] & 0xff );
        }
    }
    else {
        $data = [ @header, @body ];
    }

    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    $data->[0x01] = scalar(@$data);

    my $resp = request_with_retry( $ip, packet($data) );
    dbg( 'SET/ack', $resp );
    return 1;
}

# --- CLI ---

my %opt;
Getopt::Long::GetOptions(
    \%opt,
    'ip=s', 'protocol=s', 'get', 'set', 'debug', 'help',
    'power=s', 'mode=s', 'temperature=f', 'wind_speed=s',
    map { "$_=s" } qw[
      wind_swing_lr wind_swing_ud strong_wind comfort_sleep purifier eco dry
      prevent_cold power_saving ptc buzzer tubro comfort_power_save
      natural_wind screen_display no_wind_sense fa_no_wind_sense security
      self_clean wind_avoid intelligent_wind child_prevent_cold_wind
      little_angel cool_hot_sense gentle_wind_sense even_wind single_tuyere
      extreme_wind voice_control pre_cool_hot nobody_energy_save water_pump
      intelligent_control prevent_super_cool wind_straight
      auto_prevent_straight_wind yb_wind_avoid
    ],
) or die "bad options\n";

if ( $opt{help} || !$opt{ip} || !$opt{protocol} ) {
    print STDERR <<'USAGE';
Usage:
  ac-ng.pl --ip <addr> --protocol <model.protocol.json> --get
  ac-ng.pl --ip <addr> --protocol <model.protocol.json> --set --power on --mode cool --temperature 24

--protocol is the file written by reverse/lua/lua2map.pl for your device
model. This tool does not derive protocol knowledge itself — it only
applies what that map already found.
USAGE
    exit( $opt{help} ? 0 : 2 );
}

$DEBUG = 1 if $opt{debug};

my $map        = load_map( $opt{protocol} );
my $get_props  = build_get_props($map);
my $set_props  = build_set_props($map);

if ( $opt{get} ) {
    cmd_get( $opt{ip}, $get_props, $set_props );
}
elsif ( $opt{set} ) {
    my %want;
    for my $k ( keys %opt ) {
        next if $k =~ /^(ip|protocol|get|set|debug|help)$/;
        $want{$k} = $opt{$k} if defined $opt{$k};
    }
    die "nothing to set (no property flags given)\n" unless %want;
    cmd_set( $opt{ip}, $map->{base_frame}, $get_props, $set_props, %want );
    print "OK\n";
}
else {
    die "need --get or --set\n";
}
