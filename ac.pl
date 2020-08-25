#!/usr/bin/env perl

use strict;
use warnings;

use utf8;

use Pod::Usage   ();
use Getopt::Long ();

use POSIX ();

use List::Util  ();
use Digest::MD5 ();
use File::Spec  ();

use Socket     ();
use IO::Socket ();
use IO::Handle ();

use constant {
    RETRY          => 8,
    TIMEOUT        => 4,
    SCAN_NPROC_MAX => 4
};

use constant {
    PORT           => 6444,
    PORT_DISCOVER  => 6445,
    ADDR_DISCOVER => '255.255.255.255'
};

use constant {
    BLOCK_LEN    => 16,
    RESPONSE_LEN => 104
};

use constant {
    TEMP_MIN  => 16,
    TEMP_MAX  => 30,
    TEMP_STEP => .5
};

use constant {
    OFF => 0x00,
    ON  => 0x01
};

use constant {
    OPT_STR   => "s",
    OPT_INT   => "i",
    OPT_FLOAT => "f",
};

use constant {
    STATE_BOOLEAN => "boolean",
    STATE_VALUE  => "value"
};

use constant {
    EXIT_NORMAL => 0x00,
    EXIT_ERROR  => 0x01
};

use constant {
    WIND_SPEED => {
        AUTO       => 0x66,
        FIXED      => 0x65,
        HIGH       => 0x50,
        LOW        => 0x28,
        MIDDLE     => 0x3c,
        MUTE       => 0x14,
        RANGE_HIGH => 0x64,
        RANGE_LOW  => 0x32
    }
};

use constant {
    DELIMITER => "\n",
    SEPARATOR => ":",
    EMPTY_STR => "",
    SPACE_STR => " ",
    OUT_BEGIN => undef,
    OUT_END   => "\n",
    ESCAPES   => {
        ( 'r' => "\r", 'n' => "\n", 't' => "\t" ),
        ( map { $_ => $_ } ( '\\', '"', '$', '@' ) ),
        ( map { 'x' . unpack( 'H2', chr($_) ) => chr($_) } ( 0x00 .. 0xff ) ),
        ( map { sprintf( '%03o', $_ )         => chr($_) } ( 0x00 .. 0xff ) )
    }
};

use constant {
    PACKET => [
        0x5a, 0x5a, 0x01, 0x11, 0x68, 0x00, 0x20, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ],
    DISCOVER => [
        0xff, 0x00, 0x0e, 0x0e, 0x0e, 0x0e, 0x0e, 0x0e,
        0x0e, 0x0e, 0x0e, 0x0e, 0x0e, 0x0e, 0x0e, 0x0e,
        0x8a, 0x8d, 0xfa, 0x64, 0x70, 0xae, 0x00, 0xcf,
        0xd8, 0x5c, 0x16, 0x15, 0x96, 0xac, 0x8e
    ],
    COMMAND => [
        0xaa, 0x20, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x41, 0x81, 0x00, 0xff, 0x03, 0xff,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
};

use constant { KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S' }; # hardcoded in libEncodeAndDecodeUtils.so

use constant { KEY => Digest::MD5::md5(KEY_SIGN) };    # 6a92ef406bad2f0359baad994171ea6d

use constant {
    SETTINGS => {
        fan => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x0d] = $_[1] }
            },
            state => STATE_VALUE,
            parse => sub { $_[0]->[0x03] & 0x7f },
            val   => {
                auto       => WIND_SPEED->{AUTO},
                fixed      => WIND_SPEED->{FIXED},
                range_high => WIND_SPEED->{RANGE_HIGH},
                high       => WIND_SPEED->{HIGH},
                middle     => WIND_SPEED->{MIDDLE},
                range_low  => WIND_SPEED->{RANGE_LOW},
                low        => WIND_SPEED->{LOW},
                mute       => WIND_SPEED->{MUTE},
                map { ( $_ < 20 or $_ % 10 ) ? ( "range_" . $_ => $_ ) : () } 1 .. 99
            }
        },
        mode => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x0c] &= ~0xe0;
                    $_[0]->[0x0c] |= ( $_[1] << 0x05 ) & 0xe0;
                }
            },
            state => STATE_VALUE,
            parse => sub { ( $_[0]->[0x02] & 0xe0 ) >> 0x05 },
            val   => {
                auto => 0x01,
                cool => 0x02,
                dry  => 0x03,
                heat => 0x04,
                fan  => 0x05
            }
        },
        swing => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x11] |= 0x30;
                    $_[0]->[0x11] &= ~0x0f;
                    $_[0]->[0x11] |= $_[1];
                }
            },
            state => STATE_VALUE,

            parse => sub { $_[0]->[0x07] & 0x0f },
            val   => {
                off        => 0x00,
                vertical   => 0x0c,
                horizontal => 0x03,
                both       => 0x0f
            }
        },
        power => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x0b] &= ~0x01;
                    $_[0]->[0x0b] |= $_[1] ? ON : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x01] & 0x01 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        buzzer => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x0b] &= ~0x42;
                    $_[0]->[0x0b] |= $_[1] ? 0x42 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x0b] & 0x42 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        error => {
            parse => sub { ( $_[0]->[0x01] & 0x80 ) > OFF ? ON : OFF },
            state => STATE_BOOLEAN,
            val   => {
                no  => OFF,
                yes => ON
            }
        },
        temp => {
            input => {
                type => OPT_FLOAT,
                set  => sub {
                    $_[0]->[0x0c] &= ~0x0f;
                    $_[0]->[0x0c] |= int( $_[1] ) & 0x0f;
                    POSIX::ceil( $_[1] * 2 ) % 2 != 0 ? $_[0]->[0x0c] |= 0x10 : $_[0]->[0x0c] &= ~0x10;
                }
            },
            state => STATE_VALUE,
            parse => sub { ( $_[0]->[0x02] & 0x0f ) + 16.0 + ( $_[0]->[0x02] & 0x10 > OFF ? 0.5 : 0.0 ) },
            val   => { map { ( $_, $_ ) } map { ( $_, $_ < TEMP_MAX ? $_ + TEMP_STEP : () ) } TEMP_MIN .. TEMP_MAX }
        },
        eco => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x13] = $_[1] ? 0xff : OFF }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x09] & 0x10 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        turbo => {
            input => {
                type => OPT_STR,
                set => sub {
                    $_[0]->[0x12] &= ~0x20;
                    $_[0]->[0x12] |= 0x20 if $_[1];
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x08] & 0x20 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        led => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[1] ? $_[0]->[0x14] |= 0x10 : $_[0]->[0x14] &= ~0x10;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x0a] & 0x10 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        unit => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x14] &= ~0x04;
                    $_[0]->[0x14] |= ( $_[1] << 0x02 ) & 0x04;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x0a] & 0x04 ) >> 0x02 },
            val   => {
                "C" => OFF,
                "F" => ON
            }
        },
        temp_int => {
            state => STATE_VALUE,
            parse => sub { ( $_[0]->[0x0b] - 0x32 ) / 0x02 },
        },
        temp_ext => {
            state => STATE_VALUE,
            parse => sub { ( $_[0]->[0x0c] - 0x32 ) / 0x02 },
        },
    }
};

use constant {
    SETTINGS_VAL => {
        map {
            my $type = $_;
            (
                $type => {
                    val => {
                        exists SETTINGS->{$type}->{val}
                        ? map { ( SETTINGS->{$type}->{val}->{$_}, $_ ) }
                          keys %{ SETTINGS->{$type}->{val} }
                        : ()
                    }
                }
            )
        } keys %{ +SETTINGS }
    }
};

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

use constant {
    ALTERNATIVES => {
        'Crypt::Mode::ECB' => {
            bins => [qw(echo xxd openssl)],
            subs => {
                encrypt => 'encrypt_openssl',
                decrypt => 'decrypt_openssl'
            }
        }
    }
};

use constant {
    SELF_BIN => $0,
    (map {
        map {
            my $sub = uc($_) . '_BIN';
            defined *{"main::${sub}"} ? () : ( $sub => $_ )
        } @{ ALTERNATIVES->{$_}->{bins} }
    } keys %{ +ALTERNATIVES } )
};

sub ahex {
    my ($item) = @_;
    if ( ref($item) eq "ARRAY" ) {
        return ( join EMPTY_STR, map { sprintf( "%.2x", $_ ) } @{$item} );
    }
    elsif ( not ref($item) ) {
        return unpack( "H*", $item );
    }
}

sub module_installed {
    return eval { require File::Spec->catfile( split( /:{2}/, $_[0] ) ) . '.pm' };
}

sub which {
    return grep { -f and -x } map { File::Spec->catfile( $_, $_[0] ) } split /:/, $ENV{PATH};
}

sub inflate {
    return [ map { ord } split //, $_[0] ];
}

sub deflate {
    return join EMPTY_STR, map { defined($_) ? chr($_) : 0x00 } @{ $_[0] };
}

sub encrypt {
    return inflate( Crypt::Mode::ECB->new( AES => 0 )->encrypt( deflate( $_[0] ), KEY ) );
}

sub decrypt {
    return inflate( Crypt::Mode::ECB->new( AES => 0 )->decrypt( deflate( $_[0] ), KEY ) );
}

sub encrypt_openssl {
    my $cmd = join SPACE_STR, ECHO_BIN(), qw(-n), ahex( $_[0] ), qw(|), XXD_BIN(),
      qw(-r -p |), OPENSSL_BIN(), qw(enc -e -nopad -aes-128-ecb -K), ahex(KEY),
      qw(-in - -out - |), XXD_BIN(), qw(-p);
    return inflate pack( "H*", join EMPTY_STR, split /\n/, qx($cmd) );
}

sub decrypt_openssl {
    my $cmd = join SPACE_STR, ECHO_BIN(), qw(-n), ahex( $_[0] ), qw(|), XXD_BIN(),
      qw(-r -p |), OPENSSL_BIN(), qw(enc -d -nopad -aes-128-ecb -K), ahex(KEY),
      qw(-in - -out - |), XXD_BIN(), qw(-p);
    return inflate pack( "H*", join EMPTY_STR, split /\n/, qx($cmd) );
}

sub crc8 {
    my $crc = 0;
    $crc = CRC8_TABLE->[ $crc ^ $_ ] for @{ $_[0] };
    return $crc;
}

sub unescape {
    my $str = shift // EMPTY_STR;
    $str =~ s{(\A|\G|[^\\])[\\]([0]\d\d|[x][\da-fA-F]{2}|.)}{$1.(ESCAPES->{lc $2})}sgex;
    return $str;
}

sub quote {
    my ( $value, %opts ) = @_;
    return
      ( exists $opts{unquote_num}
          and Scalar::Util::looks_like_number($value) ) ? $value
      : length($value) ? sprintf( "%s%s%s",
        unescape( $opts{quote} ) // EMPTY_STR,
        $value, unescape( $opts{quote} ) // EMPTY_STR )
      : EMPTY_STR;
}

sub aton {
    my $mask = 0;

    if ( $_[0] =~ m{^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$} ) {
        my $b = 24;
        foreach ( $1, $2, $3, $4 ) {
            return undef if $_ > 0xff || $_ < 0x00;
            $mask += $_ << $b;
            $b    -= 8;
        }
        if ( $_[1] ) {
            my $wc;
            $mask = ~$mask if ( $mask & ( 1 << 31 ) ) == 0;
            for ( 0 .. 31 ) {
                if ( ( $mask & ( 1 << ( 31 - $_ ) ) ) == 0 ) {
                    $wc = 1;
                }
                elsif ($wc) {
                    return undef;
                }
            }
        }
        return $mask;
    }

    if ( $_[0] =~ m{^\/?(\d{1,2})$} ) {
        return undef if $1 < 1 || $1 > 32;
        $mask |= 1 << ( 31 - $_ ) for ( 0 .. $1 - 1 );
        return $mask;
    }

    return undef;
}

sub ntoa {
    return join ".", unpack( "CCCC", pack( "N", $_[0] ) );
}

sub parse_net {
    my $result = {};

    if ( $_[0] =~ m{^(.+?)\/(.+)$} ) {
        $result->{address} = aton( $1, 0 );
        $result->{netmask} = aton( $2, 1 );
    }
    elsif ( $_[0] =~ m{^(.+)\/$} ) {
        $result->{address} = aton( $1, 0 );
        $result->{netmask} = aton( 32, 0 );
    }
    else {
        $result->{address} = aton( $_[0], 0 );
        $result->{netmask} = aton( 32,    0 );
    }

    return undef unless $result->{address};

    $result->{mask_len} = 0;
    while (
        ( $result->{netmask} & ( 1 << ( 31 - $result->{mask_len} ) ) ) != 0 )
    {
        last if $result->{mask_len} > 31;
        $result->{mask_len}++;
    }

    my $network   = $result->{address} & $result->{netmask};
    my $broadcast = $network | ( ( ~$result->{netmask} ) & 4294967295 );

    $result->{wildcard} = ~$result->{netmask};

    $result->{host_min} = $network + 1;
    $result->{host_max} = $broadcast - 1;
    $result->{total}    = $result->{host_max} - $result->{host_min} + 1;

    if ( $result->{mask_len} == 31 ) {
        $result->{host_max} = $broadcast;
        $result->{host_min} = $network;
        $result->{total}    = 2;
    }
    elsif ( $result->{mask_len} == 32 ) {
        $result->{total} = 1;
        $result->{host}  = $network;
    }
    else {
        $result->{network}   = $network;
        $result->{broadcast} = $broadcast if $result->{mask_len} < 31;
    }

    return $result;
}

sub parse_net_addr {
    return [ map { parse_net($_) } split /\-/, $_[0] ];
}

sub get_cmd {
    my $data = [ @{ +COMMAND } ];
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    return $data;
}

sub set_cmd {
    my ($settings) = @_;

    my $data = [ @{ +COMMAND } ];

    $data->[0x01] = 0x23;
    $data->[0x09] = 0x02;
    $data->[0x0a] = 0x40;
    push @{$data}, (0x00) x 3;

    for ( keys %{$settings} ) {
        SETTINGS->{$_}->{input}->{set}->( $data, $settings->{$_} )
          if exists SETTINGS->{$_}->{input}->{set};
    }

    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );

    return $data;
}

sub discover_cmd {
    my $data = [ @{ +DISCOVER } ];
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    return $data;
}

sub packet {
    my ($command, %fields) = @_;
    my $packet = [ @{ +PACKET } ];

    $packet->[$_] = $fields{$_} for ( keys %fields );

    push @{$command}, ( ( ~List::Util::sum0( @{$command}[ 0x01 .. $#$command ] ) + 0x01 ) & 0xff );

    my $pad = BLOCK_LEN - ( scalar( @{$command} ) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt($command) };

    $packet->[0x04] = scalar( @{$packet} ) + BLOCK_LEN;

    push @{$packet}, @{ inflate Digest::MD5::md5( deflate($packet) . KEY_SIGN ) };

    return $packet;
}

sub set_packet {
    return packet( set_cmd( $_[0] ) );
}

sub status_packet {
    return packet( get_cmd() );
}

sub discover_packet {
    return packet( discover_cmd(), 0x06 => 0x92 );
}

sub device_settings {
    my ($data) = @_;

    die "unknown response: " . _hex($data)
      unless defined $data->[0x0a] and $data->[0x0a] == 0xc0;

    my $body = [ @{$data}[ 0x0a .. $#$data ] ];

    return {
        map {
            exists SETTINGS->{$_}->{parse}
              ? do {
                my $value = SETTINGS->{$_}->{parse}->($body);
                die "unknown \"$_\" value: \"$value\""
                  if scalar keys %{ SETTINGS_VAL->{$_}->{val} }
                  and not exists SETTINGS_VAL->{$_}->{val}->{$value};
                ( $_, $value );
              }
              : ()
        } keys %{ +SETTINGS }
    };
}

sub vals {
    my ( $data, %opts ) = @_;
    return join EMPTY_STR,
      exists $opts{begin} ? unescape( $opts{begin} ) : OUT_BEGIN // EMPTY_STR,
      (
        join exists $opts{delimiter} ? unescape( $opts{delimiter} ) : DELIMITER,
        map {
            sprintf( "%s%s%s",
                quote( $opts{value} ? EMPTY_STR : $_, %opts ),
                exists $opts{separator}
                ? unescape( $opts{separator} )
                : ( exists $opts{value} ? EMPTY_STR : SEPARATOR ),
                quote( $data->{$_}, %opts ) )
          }
          sort keys %{$data}
      ),
      exists $opts{end} ? unescape( $opts{end} ) : OUT_END // EMPTY_STR;
}

sub settings_val {
    my ($data) = @_;
    return { map { ( $_, SETTINGS_VAL->{$_}->{val}->{ $data->{$_} } // $data->{$_} ) } keys %{$data} };
}

sub settings_str {
    my ( $data, $fields, %opts ) = @_;

    my $settings = settings_val($data);

    return vals(
        {
            map { ( $_, $settings->{$_} ) } grep {
                my $field = $_;
                scalar @{$fields} ? grep { $field eq $_ } @{$fields} : $field
            } keys %{$settings}
        },
        %opts
    );
}

sub settings {
    my ( $new, $old ) = @_;

    return {
        map {
            (
                $_,
                (
                    (
                              exists( $new->{$_} )
                          and exists( SETTINGS->{$_}->{val}->{ $new->{$_} } )
                    )
                    ? SETTINGS->{$_}->{val}->{ $new->{$_} }
                    : $old->{$_}
                )
            )
        } grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
    };
}

sub discover_response {
    my ( $data ) = @_;
    if (   ( $data->[0x00] == 0x5a and $data->[0x01] == 0x5a )
        or ( $data->[0x08] == 0x5a and $data->[0x0a] == 0x5a ) )
    {
        $data = [ @{$data}[ 0x08 .. $#$data - 16 ] ]
          if $data->[0x08] == 0x5a and $data->[0x0a] == 0x5a;

        $data = decrypt( [ @{$data}[ 0x28 .. $#$data ] ] );

        $data = [ @{$data}[ 0x00 .. $#$data - $data->[-1] ] ];

        return {
          id => deflate ( [ @{$data}[ 0x29 .. 0x33 ] ] ),
          sn => deflate ( [ @{$data}[ 0x0e .. 0x23 ] ] )
        };
    }
}

sub net_request {
    my ( $device_ip, $data ) = @_;

    my $client = IO::Socket->new(
        Domain   => IO::Socket::AF_INET,
        Type     => IO::Socket::SOCK_STREAM,
        Proto    => 'tcp',
        Timeout  => TIMEOUT,
        ReusePort => 1,
        PeerPort => PORT,
        PeerHost => $device_ip
    ) or die "Socket error: $@";

    $client->send( deflate $data) == scalar @{$data}
      or $client->close(), die "Send error";

    $client->recv( my $buffer, RESPONSE_LEN );

    $client->close();

    my $len = length($buffer);

    die "no response" unless $len;

    return [ @{ inflate $buffer}[ 0x28 .. ( ( $len == 0x58 ) ? 0x47 : 0x57 ) ] ];
}

sub net_port_check {
    my $client = IO::Socket->new(
        Domain   => IO::Socket::AF_INET,
        Type     => IO::Socket::SOCK_STREAM,
        Proto    => 'tcp',
        Timeout  => POSIX::ceil( TIMEOUT / 2 ),
        ReusePort => 1,
        PeerHost => $_[0],
        PeerPort => $_[1],
    ) or return undef;

    $client->close();

    return 1;
}

sub net_discover {
    if ( net_port_check( $_[0], PORT ) ) {

        my $client = IO::Socket->new(
            Domain    => IO::Socket::AF_INET,
            Type      => IO::Socket::SOCK_DGRAM,
            Proto     => 'udp',
            Timeout   => TIMEOUT,
            ReusePort => 1,
            PeerHost  => $_[0],
            PeerPort  => PORT_DISCOVER,
        ) or die "Socket error: $@";

        $client->send(deflate discover_packet());

        $client->recv( my $buffer, RESPONSE_LEN );

        $client->close();

        if ( length($buffer) ) {
            if ( my $data = discover_response( inflate $buffer ) ) {
                return {
                  address => $_[0],
                  %{ $data }
                }
            }
        }

    }

    return undef;
}

sub net_discover_broadcast {
    my $found = [];

    my $client = IO::Socket->new(
        Domain    => IO::Socket::AF_INET,
        Type      => IO::Socket::SOCK_DGRAM,
        Proto     => 'udp',
        Timeout   => TIMEOUT * 2,
        ReusePort => 1,
        Broadcast => 1,
    ) or die "Socket error: $@";

    $client->send( deflate( discover_packet() ), 0, Socket::pack_sockaddr_in( PORT_DISCOVER, Socket::inet_aton( ADDR_DISCOVER ) ) );

    for (;;) {
        my ($buffer, $peer);

        eval {
            local $SIG{ALRM} = sub { die "timeout" };
            alarm TIMEOUT;
            $peer = $client->recv( $buffer, RESPONSE_LEN, 0 );
            alarm 0;
        };

        alarm 0;

        last if $@;

        if ( defined( $peer ) and length( $buffer ) ) {
            my $addr = Socket::inet_ntoa((Socket::unpack_sockaddr_in($peer))[1]);

            next if grep { $_->{address} eq $addr } @{ $found };

            my $data = inflate $buffer;

            if ( my $data = discover_response( inflate $buffer ) ) {
                push @{ $found }, {
                  address => $addr,
                  %{ $data }
                }
            }
        }
    }

    $client->close();

    return $found;
}

sub send_request {
    my ( $device_ip, $data ) = @_;
    my $response = decrypt( net_request( $device_ip, $data ) );
    return [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
}

sub request {
    my ( $device_ip, $packet ) = @_;

    my $last_error;
    my $retry = RETRY;

    while ( --$retry ) {
        my $data =
          eval { device_settings( send_request( $device_ip, $packet ) ) }
          or $last_error = $@, next;
        return $data;
    }

    die $last_error;
}

sub update {
    return request( $_[0], set_packet( $_[1] ) );
}

sub fetch {
    return request( $_[0], status_packet() );
}

sub scan {
    my ( $ip_address, $progress_cb ) = @_;

    return net_discover_broadcast() if $ip_address eq "255.255.255.255";

    my $net_address_list = parse_net_addr($ip_address);
    my $net_address      = shift @{$net_address_list};

    if ($net_address) {
        $net_address->{host_min} = $net_address->{host} if exists $net_address->{host};

        if ( scalar @{$net_address_list} ) {
            $net_address->{host_max} = $net_address_list->[0]->{host} // $net_address_list->[0]->{host_max};
        }
        elsif ( exists $net_address->{host} ) {
            $net_address->{host_max} = $net_address->{host};
        }

        my @hosts = $net_address->{host_min} .. $net_address->{host_max};
        $net_address->{total} = scalar @hosts;

        my $scan_nproc =
          ( SCAN_NPROC_MAX > $net_address->{total} )
          ? $net_address->{total}
          : SCAN_NPROC_MAX;

        my $batch_size = POSIX::ceil( $net_address->{total} / $scan_nproc );

        socketpair( my $child, my $parent, Socket::AF_UNIX, Socket::SOCK_STREAM, Socket::PF_UNSPEC )
          or die "socketpair error: $!";

        $child->autoflush(1);
        $parent->autoflush(1);

        for ( 0 .. $scan_nproc - 1 ) {
            my @hosts_to_scan = splice @hosts, 0, $batch_size;
            last unless scalar @hosts_to_scan;

            unless ( my $pid = fork() ) {
                die "cannot fork: $!" unless defined $pid;
                close $child;

                for (@hosts_to_scan) {
                    if ( my $result = net_discover( ntoa($_) ) ) {
                        print $parent sprintf(
                            "{%s}\n",
                            join ",",
                            (
                                map {
                                    sprintf( qq(%s=>"%s"), $_, $result->{$_} )
                                } sort keys %{$result}
                            )
                        );
                    }
                    else {
                        print $parent sprintf( qq({done=>"%s"}\n), $_ );
                    }
                }

                close $parent;
                exit EXIT_NORMAL;
            }
        }

        close $parent;

        my $scans = 0;
        my $found = [];

        until ( waitpid( -1, POSIX::WNOHANG ) == -1 ) {
            while ( my $line = <$child> ) {
                $scans++;
                chomp($line);
                if ( $line =~ m{done} ) {
                    $progress_cb->( POSIX::ceil( $scans / ( $net_address->{total} * 0.01 ) ) ) if ref $progress_cb eq "CODE";
                }
                else {
                    push @{$found}, eval $line;
                }
            }
        }

        close $child;

        return $found;
    }
    else {
        Pod::Usage::pod2usage( sprintf ( <<EOT, $_[0] ) );
Invalid value for network address: "%s"
The network address can take values in the form of an
single IP address: "xxx.xxx.xxx.xxx"
or a network with netmask: "xxx.xxx.xxx.xxx/xxx.xxx.xxx.xxx"
or a network with mask length: "xxx.xxx.xxx.xxx/xx"
or specifying a range of IP addresses: "xxx.xxx.xxx.xxx-xxx.xxx.xxx.xxx"
EOT
    }
}

my $option = {};

Getopt::Long::GetOptions(
    $option,
    qw[
      help
      set
      get
      value
      unquote_num
      discover
      ip=s
      delimiter=s
      separator=s
      quote=s
      begin=s
      end=s
      exit=s
      ],
    map    { join ":", ( $_, SETTINGS->{$_}->{input}->{type} ) }
      grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
);

Pod::Usage::pod2usage(1) if exists $option->{help};

Pod::Usage::pod2usage(2)
  if ( not( exists $option->{ip} ) )
  or (  not( exists $option->{exit} or exists $option->{discover} )
    and not( exists $option->{set} or exists $option->{get} ) )
  or ( exists $option->{set}
    and not grep { exists SETTINGS->{$_}->{input} } keys %{$option} )
  or (
    exists $option->{set}
    and grep {
        my $item = $_;
        (
            exists( $option->{$item} ) and exists( SETTINGS->{$item} )
              and (
                not scalar grep { $option->{$item} eq $_ }
                keys %{ SETTINGS->{$item}->{val} }
              )
          )
          and Pod::Usage::pod2usage(
            sprintf(
qq(Invaid %s value: "%s". It can take one of the following values: [%s]),
                $item,    $option->{$item},
                join "|", sort keys %{ SETTINGS->{$item}->{val} }
            )
          )
    } grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
  );

foreach my $module ( map { split /\,/, $_ } keys %{ +ALTERNATIVES } ) {
    unless ( module_installed($module) ) {
        no strict 'refs';
        no warnings 'redefine';

        for ( @{ ALTERNATIVES->{$module}->{bins} } ) {
            my $program = $_;
            my $sub     = uc($program) . '_BIN';

            if ( defined *{"main::${sub}"} ) {
                $program = &{"main::${sub}"}();
            }

            unless ( -f $program and -x $program ) {
                if ( ($program) = which($program) ) {
                    *{"main::${sub}"} = sub () { "$program" };
                }
                else {
                    print STDERR
                      sprintf(
qq(The "%s" program was not found. Please install this program, or install the perl module: "%s"\n),
                        $_, $module );
                    exit EXIT_ERROR;
                }
            }
        }

        foreach my $sub ( keys %{ ALTERNATIVES->{$module}->{subs} } ) {
            my $alt_sub = ALTERNATIVES->{$module}->{subs}->{$sub};
            *{"main::${sub}"} = *{"main::${alt_sub}"}
              if defined *{"main::${alt_sub}"};
        }
    }
}

if ( exists $option->{discover} ) {

    $| = 1;

    my $found = scan( $option->{ip}, sub { print STDERR sprintf("%s%%...\r", $_[0]) } );

    if ( scalar @{$found} ) {
        print STDERR sprintf(
            "\rFound %d device%s:\n",
            scalar @{$found},
            ( scalar @{$found} > 1 ? "s" : EMPTY_STR )
        );
        for ( @{$found} ) {
            print STDERR "-" x 8 . "\n";
            printf( "%s\n", vals( $_, %{$option} ) );
        }
    }
    else {
        print STDERR sprintf("\rNot found\n");
    }

    exit ( scalar @{$found} ? EXIT_NORMAL : EXIT_ERROR );
}

if ( exists $option->{set} ) {
    print settings_str(
        update( $option->{ip}, settings( $option, fetch( $option->{ip} ) ) ),
        [ grep { exists SETTINGS->{$_} } keys %{$option} ],
        %{$option}
    );
}

if ( exists $option->{get} ) {
    print settings_str(
        fetch( $option->{ip} ),
        [ grep { exists SETTINGS->{$_} } keys %{$option} ],
        %{$option}
    );
}

if ( exists $option->{exit} ) {
    Pod::Usage::pod2usage(
        sprintf(
qq(Invaid exit value: "%s". It can take one of the following values: [%s]),
            $option->{exit},
            join "|",
            grep {
                exists SETTINGS->{$_}->{parse}
                  and exists SETTINGS->{$_}->{state}
                  and SETTINGS->{$_}->{state} eq STATE_BOOLEAN
            } keys %{ +SETTINGS }
        )
    ) unless exists SETTINGS->{ $option->{exit} }->{parse};

    exit( fetch( $option->{ip} )->{ $option->{exit} } == ON ? EXIT_NORMAL : EXIT_ERROR );
}

__END__

=head1 NAME

ac.pl - midea air conditioning smart network device remote control (without m-smart cloud)

=head1 SYNOPSIS

ac.pl --ip 192.168.1.2 --set --power on --mode cool --temp 20

 Options:
   --help            brief help message

   --ip              device IP address or host name

   --get             fetch device settings [default]
   --set             update device settings

   --discover        searches for compatible devices

   --power           turn power state: [on|off]
   --temp            set target temperature: [16..30]
   --mode            set operational mode: [auto|cool|dry|heat|fan]
   --fan             set fan speed: [auto|high|medium|low|silent]
   --turbo           turn turbo mode: [on|off]
   --swing           set swing mode: [off|vertical|horizontal|both]
   --eco             turn eco mode: [on|off]
   --buzzer          turn audible feedback: [on|off]

   --value           output of values alone
   --begin           beginning of the output string [default: none]
   --end             end of the output string [default: "\n"]
   --quote           use quotes [default: none]
   --unquote_num     do not use quotes for numbers
   --separator       field separator [default: ":"]
   --delimiter       fields delimiter [default: "\n"]

   --exit            exit code 0 if value ON, else exit code 1 [eco|led|error|turbo|buzzer|power]

=head1 OPTIONS

=over 4

=item B<--help>

Print a brief help message and exits.

=item B<--ip>

IP address or host name of the device

=item B<--get>

The operation of reading device parameters

By default, all fields will be displayed

For example, you can display only one field (name with value) using the following arguments:

...--get --temp

Or display only the requested fields using the following arguments:

...--get --temp --mode --power

Or display only the value of the desired field, using the following arguments:

...--get --power --value

=item B<--set>

The operation of writing device parameters

For example, you can turn on the device using the following arguments and their values:

...--set --power on

Or turn off the device using the following arguments and their values:

...--set --power off

Or at the same time as the device is turned on, set the parameters and modes of its operation, using the following arguments and their values:

...--set --power on --mode cool --swing off --fan auto --temp 20

Or simply change one of the parameters using the following arguments and their values:

...--set --temp 17

=item B<--power>

The parameter controls the power state of the device

It can take one of the following values: [on|off]

=item B<--temp>

The parameter controls the target temperature that the device will try to reach

It can take positive integer values from this range: [16..30]

=item B<--mode>

The parameter controls the operation mode of the device

It can take one of the following values: [auto|cool|dry|heat|fan]

=item B<--fan>

The parameter controls the operation mode of the device blower fan

It can take one of the following values: [auto|high|medium|low|silent]

=item B<--turbo>

The parameter controls the turbo mode of the device

It can take one of the following values: [on|off]

=item B<--swing>

The parameter controls the operation mode of the blinds of the device

It can take one of the following values: [off|vertical|horizontal|both]

=item B<--eco>

The parameter controls the eco mode of the device

It can take one of the following values: [on|off]

=item B<--buzzer>

The parameter controls the sound response mode of the device

It can take one of the following values: [on|off]

=item B<--value>

Enables output of values alone, without field names

To output a simple array of values in JSON notation, use the following combination of arguments and their values:

...--begin "[" --end "]" --delimiter "," --quote "\"" --unquote_num --value

=item B<--begin>

The string to be put at the beginning of the output

=item B<--end>

The string to be put at the end of the output

=item B<--quote>

Specifies the string that will be used at the beginning and at the end of each field name and the value itself individually

=item B<--unquote_num>

Cancels the string that will be used at the beginning and at the end of each value if the value itself is a number

To display the output as formatted JSON, use the following combination of arguments and their values:

...--begin "{\n\t" --end "\n}\n" --delimiter ",\n\t" --quote "\"" --unquote_num

For unformatted JSON output, use the following combination of arguments and their values:

...--begin "{" --end "}" --delimiter "," --quote "\"" --unquote_num

=item B<--separator>

The separator to be used between the name of the field and its value (tupple)

=item B<--delimiter>

Separator to be used between tuples

=item B<--discover>

Searches for compatible devices on the specified network

The network can take values in the form of:

single IP address:

...--discover --ip 192.168.1.1

network with mask:

...--discover --ip 192.168.1.0/255.255.255.0

network with mask length:

...--discover --ip 192.168.1.0/24

range of IP addresses:

...--discover --ip 192.168.1.1-192.168.1.8

For fast UDP broadcast searches, use the following parameters and their values:

...--discover --ip 255.255.255.255

=item B<--exit>

The exit code will be used: if the specified parameter is set to ON, then the exit code will be 0, otherwise, the exit code will be 1

It can take one of the following values: [eco|led|error|turbo|buzzer|unit|power]

To check the power status of a device when used in third party scripts, use the following combination of arguments and their values:

...--exit power && echo "ac is on"

or

...--exit power || echo "ac is off"

=back

=head1 DESCRIPTION

B<This program> allows you to control your midea air conditioning smart network device without an m-smart cloud.
Potentially, supports devices of other brands.

=cut
