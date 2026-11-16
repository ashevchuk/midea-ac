#!/usr/bin/env perl
#
# ac-ng.pl — LAN control driven directly by the Midea-cloud-issued,
# device-model-specific protocol script (lua/<model>.lua, from
# ac-check.pl --lua), run through lua/PureLua, a pure-Perl Lua 5.1
# interpreter — instead of a hand-ported byte-offset table or a generated
# protocol map. No CPAN modules, no lua/luajit at runtime.
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
use FindBin      ();
use lib "$FindBin::Bin/lua";

use PureLua::Parser;
use PureLua::Eval;
use PureLua::Stdlib;

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

# Pads, encrypts and outer-wraps a command that already ends in its own
# trailing checksum — used for a frame built by the device .lua script's
# own jsonToData/getTotalMsg, which computes that checksum itself
# (confirmed identical algorithm to the one `packet()` below adds: both
# are `(~sum(bytes[1..end]) + 1) & 0xff`). Calling `packet()` on such a
# frame would add a second, wrong checksum on top of the real one.
sub wrap_packet {
    my ($command) = @_;
    my $packet = [ @{ +PACKET } ];

    my $pad = BLOCK_LEN - ( scalar(@$command) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt($command) };
    $packet->[0x04] = scalar(@$packet) + BLOCK_LEN;
    push @{$packet}, @{ inflate md5( deflate($packet) . KEY_SIGN ) };

    return $packet;
}

sub packet {
    my ($command) = @_;
    push @{$command}, ( ( ~List::Util::sum0( @{$command}[ 0x01 .. $#$command ] ) + 0x01 ) & 0xff );
    return wrap_packet($command);
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

# --- Device .lua script engine: runs the real jsonToData/dataToJson ---

# Fixed sentinel identity for every call — not a real per-unit serial,
# just the same value this project's generator (harness.lua) and its
# luajit cross-check tests already use, confirmed not to change the frame
# bytes beyond this. Revisit if a future model's script branches on it.
use constant { DEVICE_INFO_LUA => '{ deviceSubType = 2, deviceSN = "00000000000000Q1B0" }' };

# Bootstrap baseline this model's jsonToModel needs populated on every
# control call to avoid a nil-arithmetic crash (see harness.lua's own
# BASE_CONTROL for the full rationale) — model-specific; a different
# device .lua may need a different baseline.
my %BASE_CONTROL = (
    power             => 'on',  mode          => 'cool', temperature  => 24,
    small_temperature => 0,     wind_speed    => 50,      wind_swing_ud => 'off',
    wind_swing_lr     => 'off', strong_wind   => 'off',   comfort_sleep => 'off',
    purifier          => 'off', eco           => 'off',   dry           => 'off',
    prevent_cold      => 'off', power_saving  => 'off',   ptc           => 'off',
    sleep_status      => 0,
);

# wind_speed's control-side range is a raw 1-102 value, not a name (see
# keyP["fanspeedValue"] = checkBoundary(..., 1, 102) in the device script)
# — same codes this project's old byte-offset table used.
my %WIND_SPEED_CODE = ( auto => 102, high => 80, medium => 60, low => 40, silent => 20 );

sub lua_literal {
    my ($v) = @_;
    return $v if $v =~ /^-?\d+(?:\.\d+)?$/;
    ( my $esc = $v ) =~ s/([\\"])/\\$1/g;
    return qq{"$esc"};
}

sub lua_table_literal {
    my (%h) = @_;
    return '{' . join( ', ', map { "$_ = " . lua_literal( $h{$_} ) } sort keys %h ) . '}';
}

# Runs device_src (the Midea-cloud-issued model .lua script) followed by
# driver_src (a `return ...` calling its jsonToData/dataToJson) in one
# fresh interpreter. A fresh run per call sidesteps the keyP state leak
# documented in harness.lua — this project's CLI only ever makes one such
# call per process anyway.
sub lua_run {
    my ( $device_src, $driver_src ) = @_;
    my $eval = PureLua::Eval->new;
    PureLua::Stdlib->install($eval);
    my $ast = PureLua::Parser->parse("$device_src\n$driver_src");
    my ($result) = $eval->run($ast);
    return $eval->str_value($result);
}

# Extended ("B0/B1") properties this model's own query builder knows how
# to ask for by name, and binToModel knows how to decode the TLV answer
# back into — both confirmed by grepping the device script itself
# (`queryType == "..."` in jsonToData's query branch; the matching
# `messageBytes[cursor+0] == <tag>` in binToModel's B1 decode), not
# guessed. `little_angel` and `gentle_wind_sense` are genuinely
# write-only on this model — no query tag exists for either — so they're
# left out rather than silently queried for nothing.
my @EXTENDED_QUERY_PROPS = qw[
    prevent_super_cool auto_prevent_straight_wind self_clean wind_straight
    wind_avoid yb_wind_avoid intelligent_wind no_wind_sense fa_no_wind_sense
    child_prevent_cold_wind cool_hot_sense security even_wind single_tuyere
    extreme_wind voice_control pre_cool_hot water_pump intelligent_control
    nobody_energy_save
];

# binToModel's B0/B1 decode loop walks a shared byte cursor through every
# tag in one response, advancing it per tag by whatever fixed width the
# *script* assumes for that specific tag (5 bytes for most, more for a
# few). If a tag's real on-wire width on this unit doesn't match that
# assumption, every tag *after* it in the same response reads from the
# wrong offset — confirmed live: querying 10 properties at once decoded
# the first few as plausible small numbers, then degraded into garbage
# (one property came back as the raw tag byte of the *next* one). A tag's
# own value is only reliably at the position the script expects when
# nothing before it in the same response could have been misaligned —
# i.e. when it's the *last* property asked for. So every property here is
# queried in a 2-item batch, always second, behind a single fixed, known-
# stable anchor ("self_clean": confirmed by querying it alone to decode
# to a sane 0, independent of anything else). Costs one round trip per
# property instead of a handful, but is the only arrangement confirmed
# not to silently corrupt a neighbor's value.
use constant EXTENDED_QUERY_ANCHOR => 'self_clean';

# A query body under ~9 bytes (meaning very few properties were asked
# for) trips a different, separate bug: binToModel's own early-exit guard
# is `if (#binData < 8) then return nil end`, but `#` on a table indexed
# from 0 (as every bodyBytes table here is) ignores index 0 and undercounts
# by one — confirmed identical on real luajit, not an engine quirk — so a
# single-property query (whose body is exactly 8 bytes) always measures as
# 7 and always bails out empty, decoding nothing at all. The anchor pairing
# above exists partly for this reason too: a 2-property body is long
# enough to clear this guard.

# --- Device I/O ---

sub get_status {
    my ($ip) = @_;
    my $data = [ @{ +STATUS_QUERY } ];
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    return request_with_retry(
        $ip, packet($data),
        sub { defined $_[0]->[0x0a] && ( $_[0]->[0x0a] == 0xc0 || $_[0]->[0x0a] == 0xc1 ) }
    );
}

# Builds and sends a B1 query via the device script's own jsonToData (the
# same "query" JSON shape it already supports), the same way cmd_set lets
# it pick 0x40 vs 0xb0 — no hand-rolled tag table here either. The
# returned frame already carries its own trailing checksum (getTotalMsg
# again), hence wrap_packet, not packet(). The device acks a B1 query
# with a B0/B1-typed frame, mirroring the B0 SET ack already confirmed on
# the wire. Queries one property at a time (paired behind
# EXTENDED_QUERY_ANCHOR — see the comments above @EXTENDED_QUERY_PROPS for
# why) and skips any that come back unset: dataToJson's own returned JSON
# never includes extended fields at all (confirmed live — it decodes a B1
# response's TLV tags into keyP, then wipes keyP back to nil as its own
# cleanup before returning, the same self-resetting behavior harness.lua
# already documented for this function), so this calls its `binToModel`
# directly instead — also file-scope-local, so reachable from here in the
# same chunk, and the one piece of dataToJson's preamble that actually
# populates keyP — then reads keyP immediately after, before anything has
# a chance to clear it.
sub get_extended_status {
    my ( $ip, $device_src ) = @_;
    my $device_info = DEVICE_INFO_LUA;
    my %status;

    for my $prop (@EXTENDED_QUERY_PROPS) {
        next if $prop eq EXTENDED_QUERY_ANCHOR;
        my $query_type = EXTENDED_QUERY_ANCHOR . ",$prop";

        my $hex = lua_run(
            $device_src,
            qq{local JSON = require "cjson"\n}
          . qq{local env = { deviceinfo = $device_info, query = { query_type = "$query_type" } }\n}
          . qq{return jsonToData(JSON.encode(env))}
        );

        my @bytes = unpack( 'C*', pack( 'H*', $hex ) );
        my $resp = request_with_retry(
            $ip, wrap_packet( \@bytes ),
            sub { defined $_[0]->[0x0a] && ( $_[0]->[0x0a] == 0xb0 || $_[0]->[0x0a] == 0xb1 ) }
        );

        my $json_text = lua_run(
            $device_src,
            qq{local JSON = require "cjson"\n}
          . qq{local info = string2table("} . ahex($resp) . qq{")\n}
          . qq{dataType = info[10]\n}
          . qq{local msgBytes = {}\n}
          . qq{for i = 1, #info do msgBytes[i - 1] = info[i] end\n}
          . qq{local msgLength = msgBytes[1]\n}
          . qq{local bodyLength = msgLength - keyB["BYTE_PROTOCOL_LENGTH"] - 1\n}
          . qq{local bodyBytes = {}\n}
          . qq{for i = 0, bodyLength do bodyBytes[i] = msgBytes[i + keyB["BYTE_PROTOCOL_LENGTH"]] end\n}
          . qq{binToModel(bodyBytes, nil)\n}
          . qq{return JSON.encode({ val = keyP["$prop"] })}
        );
        # A property the device didn't echo back leaves keyP[prop] nil,
        # so Lua's `{val = nil}` has no "val" key at all — cjson's own
        # ambiguous-empty-table heuristic then encodes it as "[]", not
        # "{}", so json_decode hands back an arrayref here, not a
        # hashref. Check the type rather than assume the shape.
        my $decoded = json_decode($json_text);
        $status{$prop} = $decoded->{val}
          if ref($decoded) eq 'HASH' && defined $decoded->{val};
    }

    return \%status;
}

sub cmd_get {
    my ( $ip, $device_src ) = @_;
    my $hex = ahex( get_status($ip) );
    my $device_info = DEVICE_INFO_LUA;

    my $json_text = lua_run(
        $device_src,
        qq{local JSON = require "cjson"\n}
      . qq{local env = { deviceinfo = $device_info, msg = { data = "$hex" } }\n}
      . qq{return dataToJson(JSON.encode(env))}
    );

    my $decoded = json_decode($json_text);
    my $status  = $decoded->{status} // {};

    # Extended properties are a best-effort add-on — a device that
    # rejects or doesn't answer the B1 query shouldn't take down the
    # regular status this command already has in hand.
    my $extended = eval { get_extended_status( $ip, $device_src ) };
    warn "extended properties unavailable: $@" if $@;
    %$status = ( %$status, %$extended ) if $extended;

    for my $prop ( sort keys %$status ) {
        printf "%s:%s\n", $prop, $status->{$prop};
    }
}

sub cmd_set {
    my ( $ip, $device_src, %want ) = @_;

    if ( exists $want{temperature} ) {
        my $val   = delete $want{temperature};
        my $whole = POSIX::floor($val);
        $want{temperature}       = $whole;
        $want{small_temperature} = ( $val - $whole >= 0.5 ) ? 0.5 : 0;
    }
    if ( exists $want{wind_speed} ) {
        my $val = $want{wind_speed};
        $want{wind_speed} = $WIND_SPEED_CODE{$val}
          // ( $val =~ /^\d+$/ ? $val : die "unknown wind_speed: $val\n" );
    }

    my %control      = ( %BASE_CONTROL, %want );
    my $control_lua  = lua_table_literal(%control);
    my $device_info  = DEVICE_INFO_LUA;

    my $hex = lua_run(
        $device_src,
        qq{local JSON = require "cjson"\n}
      . qq{local env = { deviceinfo = $device_info, control = $control_lua }\n}
      . qq{return jsonToData(JSON.encode(env))}
    );

    my @bytes = unpack( 'C*', pack( 'H*', $hex ) );
    my $resp = request_with_retry( $ip, wrap_packet( \@bytes ) );
    dbg( 'SET/ack', $resp );
    return 1;
}

# --- CLI ---
#
# Guarded by `unless (caller)` so t/ac_ng.t can `require` this file to
# exercise lua_run/lua_table_literal/cmd_get/cmd_set directly (against the
# real device .lua, no network) without Getopt::Long dying on missing
# --ip/--lua or the script trying to hit the network at require time.

unless (caller) {
    my %opt;
    Getopt::Long::GetOptions(
        \%opt,
        'ip=s', 'lua=s', 'get', 'set', 'debug', 'help',
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

    if ( $opt{help} || !$opt{ip} || !$opt{lua} ) {
        print STDERR <<'USAGE';
Usage:
  ac-ng.pl --ip <addr> --lua <model.lua> --get
  ac-ng.pl --ip <addr> --lua <model.lua> --set --power on --mode cool --temperature 24

--lua is the Midea-cloud-issued, device-model-specific protocol script for
your device (as downloaded by ac-check.pl --lua). This tool runs its real
jsonToData/dataToJson through a pure-Perl Lua interpreter instead of
guessing byte offsets.
USAGE
        exit( $opt{help} ? 0 : 2 );
    }

    $DEBUG = 1 if $opt{debug};

    my $device_src = do {
        open my $fh, '<', $opt{lua} or die "open $opt{lua}: $!\n";
        local $/;
        <$fh>;
    };

    if ( $opt{get} ) {
        cmd_get( $opt{ip}, $device_src );
    }
    elsif ( $opt{set} ) {
        my %want;
        for my $k ( keys %opt ) {
            next if $k =~ /^(ip|lua|get|set|debug|help)$/;
            $want{$k} = $opt{$k} if defined $opt{$k};
        }
        die "nothing to set (no property flags given)\n" unless %want;
        cmd_set( $opt{ip}, $device_src, %want );
        print "OK\n";
    }
    else {
        die "need --get or --set\n";
    }
}

1;
