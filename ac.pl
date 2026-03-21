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
        MEDIUM     => 0x3c,
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
                medium     => WIND_SPEED->{MEDIUM},
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
                set  => sub { $_[0]->[0x13] &= ~0x80; $_[0]->[0x13] |= $_[1] ? 0x80 : OFF }
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
                    $_[0]->[0x12] |= ( $_[1] << 0x05 ) & 0x20;
                    $_[0]->[0x14] &= ~0x02;
                    $_[0]->[0x14] |= ( $_[1] << 0x01 ) & 0x02;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( ( ( $_[0]->[0x08] & 0x20 ) >> 0x05 == OFF ) ? ( ( $_[0]->[0x0a] & 0x02 ) >> 0x01 ) : ( ( $_[0]->[0x08] & 0x20 ) >> 0x05 )) > OFF ? ON : OFF },
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
        sleep => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x14] &= ~0x01;
                    $_[0]->[0x14] |= $_[1] ? 0x01 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x0a] & 0x01 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
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

use constant { SELF_BIN => $0 };

our $DEBUG = 0;


sub ahex {
    my ($item) = @_;
    if ( ref($item) eq "ARRAY" ) {
        return ( join EMPTY_STR, map { sprintf( "%.2x", $_ ) } @{$item} );
    }
    elsif ( not ref($item) ) {
        return unpack( "H*", $item );
    }
}

sub dbg {
    return unless $DEBUG;
    my ( $label, $data ) = @_;
    use feature 'state';
    state $i = 0;
    printf STDERR "[DUBUG %03d]%-10s (%3d bytes):%s\n", $i++, $label, scalar( @{$data} ), ahex($data);
}

# Packet dissection stuff
{
    my @_MODE  = ( 'auto', 'cool', 'dry', 'heat', 'fan' );
    my %_FAN   = ( 20 => 'silent', 40 => 'low', 60 => 'med', 80 => 'high', 100 => 'turbo', 102 => 'auto' );
    my %_SWING = ( 0 => 'off', 12 => 'full', 4 => 'pos1', 5 => 'pos2', 6 => 'pos3', 7 => 'pos4', 8 => 'pos5' );

    sub _dline {
        my ( $name, $fmt, @args ) = @_;
        printf STDERR "              %-22s " . $fmt . "\n", $name, @args;
    }

    sub _dissect_outer {
        my ($d) = @_;
        return unless @$d >= 6;
        printf STDERR "          --- outer packet ---\n";
        _dline( 'magic',    '%s',    sprintf( '%02x%02x', $d->[0], $d->[1] ) );
        _dline( 'length',   '%d',    $d->[2] | ( $d->[3] << 8 ) ) if @$d >= 4;
        _dline( 'msg_type', '0x%02x', $d->[8] ) if @$d >= 9;
        if ( @$d >= 10 ) {
            _dline( 'device_id_sn', '%s', sprintf( '%02x%02x%02x%02x%02x%02x%02x%02x',
                map { $d->[$_] } 4..11 ) );
        }
        _dline( 'payload_len', '%d', @$d - 36 ) if @$d > 36;
    }

    sub _dissect_cmd_body {
        my ($d, $off) = @_;
        $off //= 0;
        # body relative to the AA payload start (after AA header)
        # typical: d[0]=0xAA, d[1]=len, d[2]=dev_type, d[7]=proto, d[8]=msgtype, d[9..]=body
        my $b = $off;  # base offset into $d for body[0]
        printf STDERR "          --- command body ---\n";
        _dline( 'msg_type',  '0x%02x', $d->[$b+1] );
        return unless @$d > $b+3;
        my $b1 = $d->[$b+2];
        _dline( 'power',     '%s',     ($b1 & 0x01) ? 'on' : 'off' );
        _dline( 'buzzer',    '%s',     ($b1 & 0x02) ? 'on' : 'off' );
        return unless @$d > $b+4;
        my $b2 = $d->[$b+3];
        my $mode_idx = ($b2 >> 5) & 0x07;
        _dline( 'mode',      '%s(%d)', $_MODE[$mode_idx] // '?', $mode_idx );
        my $temp = ( ($b2 & 0x0f) + 16 ) + ( ($b2 & 0x10) ? 0.5 : 0 );
        _dline( 'set_temp',  '%.1f C', $temp );
        return unless @$d > $b+5;
        my $b3 = $d->[$b+4];
        my $fan = $b3 & 0x7f;
        _dline( 'fan_speed', '%s(%d)', $_FAN{$fan} // 'custom', $fan );
        return unless @$d > $b+9;
        my $b7 = $d->[$b+8];
        my $sw = $b7 & 0x0f;
        _dline( 'swing',     '%s(%d)', $_SWING{$sw} // 'custom', $sw );
        return unless @$d > $b+10;
        my $b8 = $d->[$b+9];
        _dline( 'feel_own',    '%s', ($b8 & 0x80) ? 'on' : 'off' );
        _dline( 'power_saver', '%s', ($b8 & 0x40) ? 'on' : 'off' );
        _dline( 'turbo(b8)',   '%s', ($b8 & 0x20) ? 'on' : 'off' );
        _dline( 'low_freq_fan','%s', ($b8 & 0x10) ? 'on' : 'off' );
        return unless @$d > $b+11;
        my $b9 = $d->[$b+10];
        _dline( 'eco',         '%s', ($b9 & 0x80) ? 'on' : 'off' );
        _dline( 'dry_clean',   '%s', ($b9 & 0x04) ? 'on' : 'off' );
        _dline( 'wise_eye',    '%s', ($b9 & 0x01) ? 'on' : 'off' );
        return unless @$d > $b+12;
        my $b10 = $d->[$b+11];
        _dline( 'turbo(b10)',  '%s', ($b10 & 0x02) ? 'on' : 'off' );
        _dline( 'sleep_func',  '%s', ($b10 & 0x01) ? 'on' : 'off' );
        _dline( 'temp_unit',   '%s', ($b10 & 0x04) ? 'F' : 'C' );
    }

    sub _dissect_resp_body {
        my ($d, $off) = @_;
        $off //= 0;
        printf STDERR "          --- response body ---\n";
        return unless @$d > $off+2;
        my $b1 = $d->[$off+2];
        _dline( 'power',       '%s', ($b1 & 0x01) ? 'on' : 'off' );
        _dline( 'buzzer',      '%s', ($b1 & 0x02) ? 'on' : 'off' );
        return unless @$d > $off+3;
        my $b2 = $d->[$off+3];
        my $mode_idx = ($b2 >> 5) & 0x07;
        _dline( 'mode',        '%s(%d)', $_MODE[$mode_idx] // '?', $mode_idx );
        my $temp = ( ($b2 & 0x0f) + 16 ) + ( ($b2 & 0x10) ? 0.5 : 0 );
        _dline( 'set_temp',    '%.1f C', $temp );
        return unless @$d > $off+4;
        my $b3 = $d->[$off+4];
        my $fan = $b3 & 0x7f;
        _dline( 'fan_speed',   '%s(%d)', $_FAN{$fan} // 'custom', $fan );
        return unless @$d > $off+8;
        my $b7 = $d->[$off+8];
        my $sw = $b7 & 0x0f;
        _dline( 'swing',       '%s(%d)', $_SWING{$sw} // 'custom', $sw );
        return unless @$d > $off+9;
        my $b8 = $d->[$off+9];
        _dline( 'feel_own',    '%s', ($b8 & 0x80) ? 'on' : 'off' );
        _dline( 'power_saver', '%s', ($b8 & 0x40) ? 'on' : 'off' );
        _dline( 'turbo',       '%s', (($b8 & 0x20) || ((@$d > $off+11) && ($d->[$off+11] & 0x02))) ? 'on' : 'off' );
        return unless @$d > $off+10;
        my $b9 = $d->[$off+10];
        _dline( 'eco',         '%s', ($b9 & 0x80) ? 'on' : 'off' );
        _dline( 'dry_clean',   '%s', ($b9 & 0x04) ? 'on' : 'off' );
        _dline( 'wise_eye',    '%s', ($b9 & 0x01) ? 'on' : 'off' );
        return unless @$d > $off+11;
        my $b10 = $d->[$off+11];
        _dline( 'sleep_func',  '%s', ($b10 & 0x01) ? 'on' : 'off' );
        _dline( 'temp_unit',   '%s', ($b10 & 0x04) ? 'F' : 'C' );
        return unless @$d > $off+12;
        my $ind = ($d->[$off+12] - 50) / 2;
        _dline( 'indoor_temp', '%.1f C', $ind );
        return unless @$d > $off+13;
        my $out = ($d->[$off+13] - 50) / 2;
        _dline( 'outdoor_temp','%.1f C', $out );
    }

    sub dissect_packet {
        return unless $DEBUG;
        my ($d, $label) = @_;
        return unless ref($d) eq 'ARRAY' && @$d >= 2;
        $label //= '';
        printf STDERR "          [dissect %s]\n", $label if $label;
        if ( $d->[0] == 0x5a && $d->[1] == 0x5a ) {
            _dissect_outer($d);
        }
        elsif ( $d->[0] == 0xaa ) {
            my $msgtype = $d->[10] // 0;
            if ( $msgtype == 0x40 || $msgtype == 0x41 ) {
                # SET or QUERY command — body[1] starts at d[11], $off=9 so $d[$off+2]=d[11]
                _dissect_cmd_body( $d, 9 );
            }
            elsif ( $msgtype == 0xc0 || $msgtype == 0xc1 ) {
                # response — body[1] starts at d[11], $off=9 so $d[$off+2]=d[11]
                _dissect_resp_body( $d, 9 );
            }
        }
    }
}

# Pure Perl AES-128-ECB (no external deps needed)
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

    # GF(2^8) multiply
    sub _mul {
        my ($a, $b) = @_;
        my $r = 0;
        for ( 1 .. 8 ) {
            $r ^= $a if $b & 1;
            my $h = $a & 0x80;
            $a = ( $a << 1 ) & 0xff;
            $a ^= 0x1b if $h;
            $b >>= 1;
        }
        $r;
    }

    # AES-128 key expansion → 44 32-bit words
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
        \@w;
    }

    # AddRoundKey: state is column-major 16 bytes (s[r + 4*c])
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
            $s[$_] = $S[$s[$_]] for 0 .. 15;             # SubBytes
            @s[1,5,9,13]  = @s[5,9,13,1];                # ShiftRows row1
            @s[2,6,10,14] = @s[10,14,2,6];               # ShiftRows row2
            @s[3,7,11,15] = @s[15,3,7,11];               # ShiftRows row3
            for my $c ( 0 .. 3 ) {                        # MixColumns
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
        \@s;
    }

    sub _dec_block {
        my ( $in, $w ) = @_;
        my @s = @$in;
        _add_rk( \@s, $w, 10 );
        for my $rnd ( reverse 1 .. 9 ) {
            @s[1,5,9,13]  = @s[13,1,5,9];                # InvShiftRows row1
            @s[2,6,10,14] = @s[10,14,2,6];               # InvShiftRows row2
            @s[3,7,11,15] = @s[7,11,15,3];               # InvShiftRows row3
            $s[$_] = $Si[$s[$_]] for 0 .. 15;            # InvSubBytes
            _add_rk( \@s, $w, $rnd );
            for my $c ( 0 .. 3 ) {                        # InvMixColumns
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
        \@s;
    }

    sub encrypt_ecb {
        my ( $data, $key ) = @_;
        my @b = unpack( 'C*', $data );
        my $w = _key_expand($key);
        my $out = '';
        $out .= pack( 'C*', @{ _enc_block( [@b[$_*16 .. $_*16+15]], $w ) } )
            for 0 .. $#b / 16;
        $out;
    }

    sub decrypt_ecb {
        my ( $data, $key ) = @_;
        my @b = unpack( 'C*', $data );
        my $w = _key_expand($key);
        my $out = '';
        $out .= pack( 'C*', @{ _dec_block( [@b[$_*16 .. $_*16+15]], $w ) } )
            for 0 .. $#b / 16;
        $out;
    }

    1;
}

sub inflate {
    return [ map { ord } split //, $_[0] ];
}

sub deflate {
    return join EMPTY_STR, map { defined($_) ? chr($_) : 0x00 } @{ $_[0] };
}

sub encrypt {
    return inflate( PureAES::encrypt_ecb( deflate( $_[0] ), KEY ) );
}

sub decrypt {
    return inflate( PureAES::decrypt_ecb( deflate( $_[0] ), KEY ) );
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
    $data->[0x08] = 0x03;          # protocol v3 for write commands (DataBodyDevOld updateProtocol)
    $data->[0x0b] = 0x02 | 0x40;  # body[1] base = 0x42 (remoteControlCode + keyStatus, from SwitchBean defaults)
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
      unless defined $data->[0x0a] and ($data->[0x0a] == 0xc0 or $data->[0x0a] == 0xc1);

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

    dbg( ">> TX", $data );
    dissect_packet( $data, '>> TX' );

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

    my $raw = inflate $buffer;
    dbg( "<< RX", $raw );

    my $enc = [ @{$raw}[ 0x28 .. ( ( $len == 0x58 ) ? 0x47 : 0x57 ) ] ];
    dbg( "<< RX/enc", $enc );

    return $enc;
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

        my $dpkt = discover_packet();
        dbg( ">> DISC TX", $dpkt );
        $client->send(deflate $dpkt);

        $client->recv( my $buffer, RESPONSE_LEN );

        $client->close();

        if ( length($buffer) ) {
            dbg( "<< DISC RX", inflate $buffer );
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

    my $bpkt = discover_packet();
    dbg( ">> BCAST TX", $bpkt );
    $client->send( deflate($bpkt), 0, Socket::pack_sockaddr_in( PORT_DISCOVER, Socket::inet_aton( ADDR_DISCOVER ) ) );

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

            dbg( "<< BCAST RX", inflate $buffer );

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
    dbg( "<< RX/dec", $response );
    my $unpadded = [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
    dbg( "<< RX/cmd", $unpadded );
    dissect_packet( $unpadded, '<< RX/cmd' );
    return $unpadded;
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

    return net_discover_broadcast() if $ip_address eq ADDR_DISCOVER;

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
      debug
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

$DEBUG = 1 if exists $option->{debug};

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
   --debug           print raw packets to stderr

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
   --sleep           turn sleep mode: [on|off]
   --buzzer          turn audible feedback: [on|off]

   --value           output of values alone
   --begin           beginning of the output string [default: none]
   --end             end of the output string [default: "\n"]
   --quote           use quotes [default: none]
   --unquote_num     do not use quotes for numbers
   --separator       field separator [default: ":"]
   --delimiter       fields delimiter [default: "\n"]

   --exit            exit code 0 if value ON, else exit code 1 [eco|led|error|turbo|sleep|buzzer|power]

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

=item B<--sleep>

The parameter controls the sleep mode of the device

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
