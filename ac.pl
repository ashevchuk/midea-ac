#!/usr/bin/env perl

use strict;
use warnings;

use utf8;

use Pod::Usage   ();
use Getopt::Long ();

use POSIX        ();

use List::Util   ();

use Socket       ();
use IO::Socket   ();
use IO::Handle   ();

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
    BLOCK_LEN         => 16,
    RESPONSE_LEN      => 104,
    DIAG_RESPONSE_LEN => 256
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


use constant {
    # key = (d2 << 8) | d3, where d2=wire_byte[1] (b in setFuncEnable), d3=wire_byte[0] (b2)
    # e.g. wire [0x18, 0x00] → key = (0x00 << 8) | 0x18 = 0x0018
    B5_PROPS => {
        0x0018 => 'cap_no_wind_feel',     # d2=0, d3=24
        0x0210 => 'cap_no_wind_speed',    # d2=2, d3=16
        0x0212 => 'cap_eco',              # d2=2, d3=18
        0x0213 => 'cap_eight_hot',        # d2=2, d3=19
        0x0214 => 'cap_modes',            # d2=2, d3=20
        0x0215 => 'cap_swing',            # d2=2, d3=21
        0x0216 => 'cap_power_cal',        # d2=2, d3=22
        0x0217 => 'cap_self_check',       # d2=2, d3=23
        0x0219 => 'cap_aux_heat',         # d2=2, d3=25
        0x021a => 'cap_turbo',            # d2=2, d3=26
        0x021f => 'cap_humidity_clear',   # d2=2, d3=31
        0x0222 => 'cap_unit_changeable',  # d2=2, d3=34
        0x0225 => 'cap_temp_range',       # d2=2, d3=37
    }
};

# B0/B1 property tags (midealan CapabilityTag / PropertiesDefaultQuery)
use constant {
    PROP_WIND_UD    => 0x0009,
    PROP_WIND_LR    => 0x000A,
    PROP_HUMIDITY   => 0x0015,
    PROP_DISPLAY    => 0x0017,
    PROP_BREEZELESS => 0x0018,
};

use constant {
    SWING_H_VAL => {
        off       => 0,
        left      => 1,
        left_mid  => 25,
        middle    => 50,
        right_mid => 75,
        right     => 100,
    },
    SWING_V_VAL => {
        off      => 0,
        up       => 1,
        up_mid   => 25,
        middle   => 50,
        down_mid => 75,
        down     => 100,
    },
};

# Valid indoor/outdoor sensor range (°C); outside → "n/a"
use constant {
    TEMP_SENSOR_MIN => -20,
    TEMP_SENSOR_MAX => 60,
};

use constant { KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S' }; # hardcoded in libEncodeAndDecodeUtils.so
#use constant { KEY => md5(KEY_SIGN) }; # 6a92ef406bad2f0359baad994171ea6d
use constant { KEY => "6a92ef406bad2f0359baad994171ea6d" }; # precomputed md5(KEY_SIGN): 6a92ef406bad2f0359baad994171ea6d

# AA set/get body offsets (StateSet / StateBody / DataBodyDevOld):
#   [0x0a]=msgtype  [0x0b]=power/buzzer  [0x0c]=mode/temp  [0x0d]=fan
#   [0x11]=swing  [0x12]=turbo/power_saving  [0x13]=eco/anion/dry/aux/eye
#   [0x14]=sleep/unit/led/turbo2  [0x1b]=natural_wind  [0x1f]=frost  [0x20]=comfort
# Response parse body is sliced from msgtype (body[0]=0xC0/0xC1).
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
                silent     => WIND_SPEED->{MUTE},
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
        # prompt_tone (0x40); keep keyStatus bit 0x02 untouched on clear
        buzzer => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x0b] &= ~0x40;
                    $_[0]->[0x0b] |= $_[1] ? 0x40 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x01] & 0x40 ) > OFF ? ON : OFF },
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
                    POSIX::ceil( $_[1] * 2 ) % 2 != 0
                      ? $_[0]->[0x0c] |= 0x10
                      : $_[0]->[0x0c] &= ~0x10;
                }
            },
            state => STATE_VALUE,
            parse => sub {
                ( $_[0]->[0x02] & 0x0f ) + 16.0
                  + ( ( $_[0]->[0x02] & 0x10 ) > OFF ? 0.5 : 0.0 );
            },
            val => {
                map { ( $_, $_ ) }
                  map { ( $_, $_ < TEMP_MAX ? $_ + TEMP_STEP : () ) }
                  TEMP_MIN .. TEMP_MAX
            }
        },
        # set bit7 / get bit4 — same quirk as midealan StateSet/StateBody
        eco => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x13] &= ~0x80;
                    $_[0]->[0x13] |= $_[1] ? 0x80 : OFF;
                }
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
                set  => sub {
                    $_[0]->[0x12] &= ~0x20;
                    $_[0]->[0x12] |= ( $_[1] << 0x05 ) & 0x20;
                    $_[0]->[0x14] &= ~0x02;
                    $_[0]->[0x14] |= ( $_[1] << 0x01 ) & 0x02;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub {
                (
                    (
                        ( $_[0]->[0x08] & 0x20 ) >> 0x05 == OFF
                        ? ( ( $_[0]->[0x0a] & 0x02 ) >> 0x01 )
                        : ( ( $_[0]->[0x08] & 0x20 ) >> 0x05 )
                    )
                ) > OFF ? ON : OFF;
            },
            val => {
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
        power_saving => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x12] &= ~0x08;
                    $_[0]->[0x12] |= $_[1] ? 0x08 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x08] & 0x08 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        smart_eye => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x13] &= ~0x01;
                    $_[0]->[0x13] |= $_[1] ? 0x01 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x09] & 0x01 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        dry_clean => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x13] &= ~0x04;
                    $_[0]->[0x13] |= $_[1] ? 0x04 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x09] & 0x04 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        aux_heat => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x13] &= ~0x08;
                    $_[0]->[0x13] |= $_[1] ? 0x08 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x09] & 0x08 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        anion => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x13] &= ~0x20;
                    $_[0]->[0x13] |= $_[1] ? 0x20 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub { ( $_[0]->[0x09] & 0x20 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        natural_wind => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x1b] &= ~0x40;
                    $_[0]->[0x1b] |= $_[1] ? 0x40 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            # StateBody reports this on byte9 bit1; StateSet writes byte17 bit6
            parse => sub {
                my $b = $_[0];
                ( ( $b->[0x09] // 0 ) & 0x02 ) > OFF
                  || ( ( $b->[0x11] // 0 ) & 0x40 ) > OFF ? ON : OFF;
            },
            val => {
                off => OFF,
                on  => ON
            }
        },
        frost_protect => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x1f] &= ~0x80;
                    $_[0]->[0x1f] |= $_[1] ? 0x80 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub {
                ( ( $_[0]->[0x15] // 0 ) & 0x80 ) > OFF ? ON : OFF;
            },
            val => {
                off => OFF,
                on  => ON
            }
        },
        comfort => {
            input => {
                type => OPT_STR,
                set  => sub {
                    $_[0]->[0x20] &= ~0x01;
                    $_[0]->[0x20] |= $_[1] ? 0x01 : OFF;
                }
            },
            state => STATE_BOOLEAN,
            parse => sub {
                ( ( $_[0]->[0x16] // 0 ) & 0x01 ) > OFF ? ON : OFF;
            },
            val => {
                off => OFF,
                on  => ON
            }
        },
        err_code => {
            state => STATE_VALUE,
            parse => sub { $_[0]->[0x10] },
        },
        temp_int => {
            state => STATE_VALUE,
            parse => sub {
                my $raw = $_[0]->[0x0b];
                my $t   = ( $raw - 0x32 ) / 0x02;
                return 'n/a'
                  if !defined $raw
                  || $raw == 0x00
                  || $raw == 0xff
                  || $t < TEMP_SENSOR_MIN
                  || $t > TEMP_SENSOR_MAX;
                return $t;
            },
        },
        temp_ext => {
            state => STATE_VALUE,
            parse => sub {
                my $raw = $_[0]->[0x0c];
                my $t   = ( $raw - 0x32 ) / 0x02;
                return 'n/a'
                  if !defined $raw
                  || $raw == 0x00
                  || $raw == 0xff
                  || $t < TEMP_SENSOR_MIN
                  || $t > TEMP_SENSOR_MAX;
                return $t;
            },
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
    printf STDERR "[DEBUG %03d]%-10s (%3d bytes):%s\n", $i++, $label, scalar( @{$data} ), ahex($data);
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

    sub _dissect_b5_body {
        my ($d, $off) = @_;
        $off //= 0;
        printf STDERR "          --- B5 body ---\n";
        return unless @$d > $off + 2;
        my $count = $d->[$off + 2];
        _dline( 'cap_count', '%d', $count );
        my $pos = $off + 3;
        for my $i ( 1 .. $count ) {
            last if $pos + 2 >= @$d;
            my $d3  = $d->[$pos];
            my $d2  = $d->[$pos + 1];
            my $len = $d->[$pos + 2];
            $pos += 3;
            last if $pos + $len > @$d;
            my @val = @{$d}[ $pos .. $pos + $len - 1 ];
            $pos += $len;
            my $key  = ( $d2 << 8 ) | $d3;
            my $name = B5_PROPS->{$key} // sprintf( 'cap_%02x_%02x', $d3, $d2 );
            _dline( $name, '%s', ahex( \@val ) );
        }
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
            elsif ( $msgtype == 0xb5 ) {
                _dissect_b5_body( $d, 9 );
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
        return $r;
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
        return \@w;
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
        return \@s;
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

sub md5 {
    my ($msg) = $_[0];
    my ($a, $b, $c, $d) = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476);
    use feature 'state';
    state @T = map { int(4294967296 * abs(sin($_ + 1))) } 0 .. 63;
    state @S = ( (7,12,17,22) x 4, (5, 9,14,20) x 4, (4,11,16,23) x 4, (6,10,15,21) x 4 );
    my $msg_len = length($msg);
    $msg .= pack("C", 0x80);
    $msg .= pack("C", 0) while ( length($msg) % 64 != 56 );
    $msg .= pack("V2", $msg_len * 8, 0);
    my $rotl = sub { my ($x, $n) = @_; $x &= 0xffffffff; (($x << $n) | ($x >> (32 - $n))) & 0xffffffff; };
    for (my $i = 0; $i < length($msg); $i += 64) {
        my @M = unpack('V16', substr($msg, $i, 64));
        my ($A, $B, $C, $D) = ($a, $b, $c, $d);
        for my $j ( 0 .. 63 ) {
            my ( $F, $g );
            if ( $j < 16 ) { $F = ($B & $C) | ((~$B & 0xffffffff) & $D); $g = $j; }
            elsif ( $j < 32 ) { $F = ($D & $B) | ((~$D & 0xffffffff) & $C); $g = (5 * $j + 1) % 16; }
            elsif ( $j < 48 ) { $F = $B ^ $C ^ $D; $g = (3 * $j + 5) % 16; }
            else { $F = $C ^ ($B | (~$D & 0xffffffff)); $g = (7 * $j) % 16; }
            $F = ($F + $A + $T[$j] + $M[$g]) & 0xffffffff; $A = $D; $D = $C; $C = $B; $B = ($B + $rotl->($F, $S[$j])) & 0xffffffff;
        }
        $a = ($a + $A) & 0xffffffff; $b = ($b + $B) & 0xffffffff; $c = ($c + $C) & 0xffffffff; $d = ($d + $D) & 0xffffffff;
    }
    return pack("V4", $a, $b, $c, $d);
}

sub inflate {
    return [ map { ord } split //, $_[0] ];
}

sub deflate {
    return join EMPTY_STR, map { defined($_) ? chr($_) : 0x00 } @{ $_[0] };
}

sub encrypt {
    return inflate( PureAES::encrypt_ecb( deflate( $_[0] ), pack("H*", KEY) ) );
}

sub decrypt {
    return inflate( PureAES::decrypt_ecb( deflate( $_[0] ), pack("H*", KEY) ) );
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

    $data->[0x09] = 0x02;
    $data->[0x0a] = 0x40;         # set
    $data->[0x08] = 0x03;         # device protocol (DataBodyDevOld updateProtocol)
    $data->[0x0b] = 0x02;         # keyStatus; buzzer/prompt_tone applied via SETTINGS

    # StateSet body is 22 bytes after FirstByte (indices 0x0b .. 0x20)
    my $need = 0x0b + 22;
    push @{$data}, (0x00) x ( $need - scalar @{$data} )
      if scalar @{$data} < $need;

    for ( keys %{$settings} ) {
        SETTINGS->{$_}->{input}->{set}->( $data, $settings->{$_} )
          if exists SETTINGS->{$_}->{input}->{set};
    }

    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    $data->[0x01] = scalar @{$data};

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

    push @{$packet}, @{ inflate md5( deflate($packet) . KEY_SIGN ) };

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
    my %out = map {
        ( $_, SETTINGS_VAL->{$_}->{val}->{ $data->{$_} } // $data->{$_} )
    } keys %{$data};

    # Prefer canonical "silent" over alias "mute" for the same wire value
    if ( exists $data->{fan}
        && ( ( $data->{fan} & 0x7f ) == WIND_SPEED->{MUTE} ) )
    {
        $out{fan} = 'silent';
    }

    return \%out;
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
    my ( $device_ip, $data, $recv_len ) = @_;
    $recv_len //= RESPONSE_LEN;

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
    dbg( "<< RX", $raw );

    my $enc_end = $len >= 17 ? $len - 17 : 0x27;
    $enc_end = 0x27 if $enc_end < 0x28;
    my $enc = [ @{$raw}[ 0x28 .. $enc_end ] ];
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
    my ( $device_ip, $data, $recv_len ) = @_;
    my $response = decrypt( net_request( $device_ip, $data, $recv_len ) );
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

sub diag_cmd {
    my ($additional) = @_;

    # First frame matches the OEM/working query (0x01, 0x11).
    # Second frame matches midealan CapabilitiesAdditionalQuery (0x01, 0x01, 0x01).
    my @payload =
      $additional
      ? ( 0x01, 0x01, 0x01 )
      : ( 0x01, 0x11 );

    my @body = ( 0xb5, @payload );
    push @body, crc8( \@body );
    return [
        0xaa, 0x00, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, @body
    ];
}

sub diag_packet {
    return packet( diag_cmd( $_[0] ) );
}

sub parse_b5 {
    my ($data) = @_;
    return ( {}, 0 )
      unless defined $data && scalar(@$data) > 0x0c && $data->[0x0a] == 0xb5;

    my @body  = @{$data}[ 0x0b .. $#$data ];
    my $count = $body[0];
    my %props;
    my $pos = 1;

    for my $i ( 1 .. $count ) {
        last if $pos + 2 >= scalar @body;
        my $d3  = $body[$pos];
        my $d2  = $body[ $pos + 1 ];
        my $len = $body[ $pos + 2 ];
        $pos += 3;
        last if $pos + $len > scalar @body;
        my @val = @body[ $pos .. $pos + $len - 1 ];
        $pos += $len;
        my $key  = ( $d2 << 8 ) | $d3;
        my $name = B5_PROPS->{$key} // sprintf( 'cap_%02x_%02x', $d3, $d2 );
        $props{$name} = \@val;
    }

    # Trailer [flag, msg_id, crc]: non-zero flag → second B5 frame available
    my $need_more = 0;
    if ( scalar(@body) - $pos >= 3 ) {
        $need_more = ( $body[$pos] // 0 ) ? 1 : 0;
    }

    return ( \%props, $need_more );
}

# B5 decode aligned with midealan CapabilityBody + OEM CmdB5.setFuncEnable
my %_CAP_HEAT   = map { $_ => 1 } ( 1, 2, 4, 6, 7, 9, 10, 11, 12, 13 );
my %_CAP_NOCOOL = map { $_ => 1 } ( 2, 10, 12 );
my %_CAP_DRY    = map { $_ => 1 } ( 0, 1, 5, 6, 9, 11, 13 );
my %_CAP_AUTO   = map { $_ => 1 } ( 0, 1, 2, 7, 8, 9, 13 );
my %_CAP_SWING_H = map { $_ => 1 } ( 1, 3 );
my %_CAP_FAN_LH  = map { $_ => 1 } ( 3, 4, 5, 6, 7, 9 );
my %_CAP_FAN_M   = map { $_ => 1 } ( 5, 6, 7 );
my %_CAP_FAN_A   = map { $_ => 1 } ( 4, 5, 6, 9 );
my %_CAP_FAN_S   = map { $_ => 1 } ( 6, 9 );
my %_CAP_SELF_CHECK = (
    0 => 'no',
    1 => 'yes',
    2 => 'yes+nest',
    3 => 'nest_only',
    4 => 'yes+nest_change',
);
my %_CAP_TURBO = (
    0 => 'cool',
    1 => 'heat',
    2 => 'cool',
    3 => 'heat+cool',
);

sub render_b5 {
    my ($props) = @_;
    my %out;
    while ( my ( $k, $v ) = each %$props ) {
        my $d = $v->[0] // 0;
        if ( $k eq 'cap_modes' ) {
            my @m;
            push @m, 'heat' if $_CAP_HEAT{$d};
            push @m, 'cool' unless $_CAP_NOCOOL{$d};
            push @m, 'dry'  if $_CAP_DRY{$d};
            push @m, 'auto' if $_CAP_AUTO{$d};
            $out{$k} = @m ? join( '+', @m ) : sprintf( 'raw_%d', $d );
        }
        elsif ( $k eq 'cap_swing' ) {
            my @s;
            push @s, 'horizontal' if $_CAP_SWING_H{$d};
            push @s, 'vertical'   if $d < 2;
            $out{$k} = @s ? join( '+', @s ) : 'none';
        }
        elsif ( $k eq 'cap_no_wind_speed' ) {
            my @f;
            my $custom = $d == 1;
            push @f, 'silent' if $custom || $_CAP_FAN_S{$d};
            push @f, 'low'    if $custom || $_CAP_FAN_LH{$d};
            push @f, 'medium' if $custom || $_CAP_FAN_M{$d};
            push @f, 'high'   if $custom || $_CAP_FAN_LH{$d};
            push @f, 'auto'   if $custom || $_CAP_FAN_A{$d};
            push @f, 'custom' if $custom;
            $out{cap_fan_speeds} = @f ? join( '+', @f ) : sprintf( 'raw_%d', $d );
        }
        elsif ( $k eq 'cap_self_check' ) {
            $out{$k} = $_CAP_SELF_CHECK{$d} // ahex($v);
        }
        elsif ( $k eq 'cap_eco' ) {
            $out{$k} = $d == 2 ? 'special' : ( ( $d == 1 || $d == 2 ) ? 'yes' : 'no' );
        }
        elsif ( $k eq 'cap_unit_changeable' ) {
            $out{$k} = $d == 0 ? 'yes' : 'no';
        }
        elsif ( $k eq 'cap_turbo' ) {
            # midealan: value < 2 => turbo_cool; heat set {1,3} roughly
            my @t;
            push @t, 'cool' if $d < 2;
            push @t, 'heat' if $d == 1 || $d == 3;
            $out{$k} = @t ? join( '+', @t ) : ( $_CAP_TURBO{$d} // 'no' );
        }
        elsif ( $k eq 'cap_temp_range' && scalar(@$v) >= 6 ) {
            $out{cap_cool_min} = $v->[0] / 2.0;
            $out{cap_cool_max} = $v->[1] / 2.0;
            $out{cap_auto_min} = $v->[2] / 2.0;
            $out{cap_auto_max} = $v->[3] / 2.0;
            $out{cap_heat_min} = $v->[4] / 2.0;
            $out{cap_heat_max} = $v->[5] / 2.0;
            if ( scalar(@$v) > 6 ) {
                my $dec_i = scalar(@$v) > 8 ? 8 : 6;
                $out{cap_temp_decimals} = ( $v->[$dec_i] // 0 ) ? 'yes' : 'no';
            }
        }
        elsif ( $k eq 'cap_humidity_clear' ) {
            my %h = ( 0 => 'no', 1 => 'auto', 2 => 'manual', 3 => 'auto+manual' );
            $out{$k} = $h{$d} // ahex($v);
        }
        elsif ( $k =~ /^cap_(?:no_wind_feel|eight_hot|aux_heat|power_cal)/ ) {
            $out{$k} = $d ? 'yes' : 'no';
        }
        else {
            $out{$k} = ahex($v);
        }
    }
    return \%out;
}

sub diag {
    my ($device_ip) = @_;

    my $std = eval { settings_val( fetch($device_ip) ) } // {};

    # Brief pause — back-to-back status→B5 sometimes gets no reply on this unit
    select( undef, undef, undef, 0.15 );
    my $b5_raw =
      eval { send_request( $device_ip, diag_packet(0), DIAG_RESPONSE_LEN ) };
    my ( $b5_props, $need_more ) =
      ( $b5_raw && ( $b5_raw->[0x0a] // 0 ) == 0xb5 )
      ? parse_b5($b5_raw)
      : ( {}, 0 );
    my $b5 = render_b5($b5_props);

    if ($need_more) {
        select( undef, undef, undef, 0.15 );
        my $b5b_raw =
          eval { send_request( $device_ip, diag_packet(1), DIAG_RESPONSE_LEN ) };
        if ( $b5b_raw && ( $b5b_raw->[0x0a] // 0 ) == 0xb5 ) {
            my ($extra) = parse_b5($b5b_raw);
            my $b5b = render_b5($extra);
            @{$b5}{ keys %$b5b } = values %$b5b;
        }
    }

    my $props = eval { fetch_props($device_ip) } // {};
    my $power = eval { fetch_power($device_ip) } // {};

    return { %$std, %$b5, %$props, %$power };
}

# --- B0/B1 property protocol (angles, breezeless, display, humidity) ---

sub props_query_cmd {
    my (@tags) = @_;
    @tags = (
        PROP_WIND_UD, PROP_WIND_LR, PROP_HUMIDITY,
        PROP_DISPLAY, PROP_BREEZELESS
    ) unless @tags;

    my $data = [
        0xaa, 0x00, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03,    # query
        0xb1, scalar @tags,
    ];
    push @{$data}, ( $_ & 0xff ), ( $_ >> 8 ) for @tags;
    push @{$data}, 0x01;    # message id
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    $data->[0x01] = scalar @{$data};
    return $data;
}

sub props_set_cmd {
    my (%tag_vals) = @_;

    my @packs;
    my $count = 0;
    for my $tag ( sort { $a <=> $b } keys %tag_vals ) {
        # 4-byte pack format (midealan PropertiesSet wire format)
        push @packs, ( $tag & 0xff ), ( $tag >> 8 ), 0x01, ( $tag_vals{$tag} & 0xff );
        $count++;
    }

    my $data = [
        0xaa, 0x00, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x03, 0x02,    # device protocol 3, set
        0xb0, $count, @packs,
    ];
    push @{$data}, 0x01;
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    $data->[0x01] = scalar @{$data};
    return $data;
}

sub parse_props {
    my ($data) = @_;
    return {} unless defined $data && scalar(@$data) > 0x0c;

    my $bt = $data->[0x0a];
    return {} unless $bt == 0xb0 || $bt == 0xb1;

    my @body  = @{$data}[ 0x0a .. $#$data ];
    my $count = $body[1] // 0;
    my $pos   = 2;
    my %props;

    for ( 1 .. $count ) {
        last if $pos + 2 > scalar @body;
        my $tag = $body[$pos] | ( $body[ $pos + 1 ] << 8 );
        $pos += 2;
        # B0/B1 responses use 5-byte TLV: tag_lo, tag_hi, pad, len, value…
        last if $pos >= scalar @body;
        $pos += 1;
        last if $pos >= scalar @body;
        my $len = $body[$pos++];
        last if $pos + $len > scalar @body;
        my @val = @body[ $pos .. $pos + $len - 1 ];
        $pos += $len;
        $props{$tag} = \@val;
    }

    return \%props;
}

sub _angle_name {
    my ( $map, $byte ) = @_;
    my %rev = reverse %{$map};
    return $rev{$byte} // sprintf( 'raw_%d', $byte );
}

sub fetch_props {
    my ($device_ip) = @_;

    my $raw =
      send_request( $device_ip, packet( props_query_cmd() ), DIAG_RESPONSE_LEN );
    dbg( "<< B1", $raw );
    my $props = parse_props($raw);
    my %out;

    if ( exists $props->{ +PROP_WIND_LR } ) {
        $out{swing_h} = _angle_name( SWING_H_VAL, $props->{ +PROP_WIND_LR }[0] );
    }
    if ( exists $props->{ +PROP_WIND_UD } ) {
        $out{swing_v} = _angle_name( SWING_V_VAL, $props->{ +PROP_WIND_UD }[0] );
    }
    if ( exists $props->{ +PROP_HUMIDITY } ) {
        $out{humidity} = $props->{ +PROP_HUMIDITY }[0] // 0;
    }
    if ( exists $props->{ +PROP_DISPLAY } ) {
        $out{display} =
          ( ( $props->{ +PROP_DISPLAY }[0] // 0 ) > 0 ) ? 'on' : 'off';
    }
    if ( exists $props->{ +PROP_BREEZELESS } ) {
        $out{breezeless} =
          ( ( $props->{ +PROP_BREEZELESS }[0] // 0 ) == 1 ) ? 'on' : 'off';
    }

    return \%out;
}

# Back-compat alias
sub fetch_angles { return fetch_props(@_) }

sub update_props {
    my ( $device_ip, %want ) = @_;

    my %tags;
    if ( exists $want{swing_h} ) {
        die sprintf(
            qq(Invalid swing_h value: "%s". Use: [%s]),
            $want{swing_h}, join( "|", sort keys %{ +SWING_H_VAL } )
        ) unless exists SWING_H_VAL->{ $want{swing_h} };
        $tags{ +PROP_WIND_LR } = SWING_H_VAL->{ $want{swing_h} };
    }
    if ( exists $want{swing_v} ) {
        die sprintf(
            qq(Invalid swing_v value: "%s". Use: [%s]),
            $want{swing_v}, join( "|", sort keys %{ +SWING_V_VAL } )
        ) unless exists SWING_V_VAL->{ $want{swing_v} };
        $tags{ +PROP_WIND_UD } = SWING_V_VAL->{ $want{swing_v} };
    }
    if ( exists $want{breezeless} ) {
        die qq(Invalid breezeless value: "$want{breezeless}". Use: [on|off])
          unless $want{breezeless} eq 'on' || $want{breezeless} eq 'off';
        $tags{ +PROP_BREEZELESS } = $want{breezeless} eq 'on' ? 0x01 : 0x00;
    }
    if ( exists $want{display} ) {
        die qq(Invalid display value: "$want{display}". Use: [on|off])
          unless $want{display} eq 'on' || $want{display} eq 'off';
        # midealan: 0x64 = on, 0x00 = off
        $tags{ +PROP_DISPLAY } = $want{display} eq 'on' ? 0x64 : 0x00;
    }

    return {} unless keys %tags;

    my $raw =
      send_request( $device_ip, packet( props_set_cmd(%tags) ), DIAG_RESPONSE_LEN );
    dbg( "<< B0", $raw );

    my $out = eval { fetch_props($device_ip) } // {};
    return {
        map { ( $_, $out->{$_} ) }
          grep { exists $want{$_} && exists $out->{$_} }
          qw[swing_h swing_v breezeless display]
    };
}

sub update_angles { return update_props(@_) }

# --- group_data power / energy (0x41 / response 0xC1) ---

sub group_data_cmd {
    my ($group) = @_;
    $group //= 0;

    # midealan GroupDataQuery: body_type 0x41, no message_id
    my $data = [
        0xaa, 0x00, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03,
        0x41, 0x21, 0x01, ( 0x40 | ( $group & 0x0f ) ), 0x00, 0x01,
    ];
    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );
    $data->[0x01] = scalar @{$data};
    return $data;
}

sub _power_decode_value {
    my ( $method, $bytes ) = @_;
    $method = $method % 10;
    my $value = 0;
    for my $byte (@$bytes) {
        if ( $method == 1 ) {    # BCD
            $value = ( ( $byte >> 4 ) * 10 + ( $byte & 0x0f ) ) + $value * 100;
        }
        elsif ( $method == 2 ) {    # BINARY
            $value = $byte + ( $value << 8 );
        }
        else {                      # MIXED / INT (3)
            $value = $byte + $value * 100;
        }
    }
    return $value;
}

sub _parse_power_w {
    my ( $method, $bytes ) = @_;
    # method 101: BCD energy + binary power — power side uses binary
    my $m = ( $method == 101 ) ? 2 : $method;
    return _power_decode_value( $m, $bytes ) / 10.0;
}

sub _parse_energy_kwh {
    my ( $method, $bytes ) = @_;
    if ( $method == 101 ) {
        return _power_decode_value( 1, $bytes ) / 100.0;
    }
    my $div = ( ( $method % 10 ) == 2 ) ? 10.0 : 100.0;
    return _power_decode_value( $method, $bytes ) / $div;
}

sub parse_group_power {
    my ( $data, $method ) = @_;
    $method //= 1;
    return {} unless defined $data && ( $data->[0x0a] // 0 ) == 0xc1;

    my @body = @{$data}[ 0x0a .. $#$data ];
    return {} if scalar(@body) <= 3;

    my $gtype = $body[3];
    my %out;

    if ( $gtype == 0x44 && scalar(@body) >= 19 ) {
        $out{energy_total_kwh} =
          sprintf( '%.2f', _parse_energy_kwh( $method, [ @body[ 4 .. 7 ] ] ) );
        $out{energy_operating_kwh} =
          sprintf( '%.2f', _parse_energy_kwh( $method, [ @body[ 8 .. 11 ] ] ) );
        $out{energy_current_kwh} =
          sprintf( '%.2f', _parse_energy_kwh( $method, [ @body[ 12 .. 15 ] ] ) );
        $out{power_w} =
          sprintf( '%.1f', _parse_power_w( $method, [ @body[ 16 .. 18 ] ] ) );
    }
    elsif ( $gtype == 0x40 && scalar(@body) >= 19 ) {
        my $day  = $body[5] | ( $body[4] << 8 );
        my $hour = $body[6];
        my $min  = $body[7];
        $out{electrify_hours} =
          sprintf( '%.2f', $day * 24 + $hour + $min / 60.0 );
    }

    return \%out;
}

sub fetch_power {
    my ( $device_ip, $method ) = @_;
    $method //= 1;

    my %out;
    # Group 4 (runtime) often answers when group 0 (energy/power) does not;
    # try 4 first, then merge whatever group 0 returns.
    for my $group ( 4, 0 ) {
        my $raw = eval {
            send_request(
                $device_ip,
                packet( group_data_cmd($group) ),
                DIAG_RESPONSE_LEN
            );
        };
        next unless $raw;
        dbg( "<< C1/g$group", $raw );
        my $parsed = parse_group_power( $raw, $method );
        @out{ keys %$parsed } = values %$parsed if keys %$parsed;
    }
    return \%out;
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
      caps
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
      swing_h:s
      swing_v:s
      breezeless:s
      display:s
      humidity
      temp_int
      temp_ext
      power_w
      energy_total_kwh
      energy_current_kwh
      energy_operating_kwh
      electrify_hours
      ],
    map    { join ":", ( $_, SETTINGS->{$_}->{input}->{type} ) }
      grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
);

Pod::Usage::pod2usage(1) if exists $option->{help};

$DEBUG = 1 if exists $option->{debug};

# mute is an input alias for silent
$option->{fan} = 'silent'
  if exists $option->{fan} && defined $option->{fan} && $option->{fan} eq 'mute';

my $has_prop_set =
     ( exists $option->{swing_h} and defined $option->{swing_h} and length $option->{swing_h} )
  || ( exists $option->{swing_v} and defined $option->{swing_v} and length $option->{swing_v} )
  || ( exists $option->{breezeless} and defined $option->{breezeless} and length $option->{breezeless} )
  || ( exists $option->{display} and defined $option->{display} and length $option->{display} );

# Iterate SETTINGS keys only — never `exists SETTINGS->{$opt_key}->{…}` over
# CLI keys (autovivifies and pollutes the SETTINGS hash).
my $has_c0_set = scalar grep { exists $option->{$_} }
  grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS };

Pod::Usage::pod2usage(2)
  if ( not( exists $option->{ip} ) )
  or (  not( exists $option->{exit} or exists $option->{discover} or exists $option->{caps} )
    and not( exists $option->{set} or exists $option->{get} ) )
  or ( exists $option->{set} and not( $has_c0_set || $has_prop_set ) )
  or (
    exists $option->{set}
    and grep {
        my $item = $_;
        (
            exists( $option->{$item} ) and exists( SETTINGS->{$item} )
              and defined( $option->{$item} )
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
  )
  or (
    exists $option->{set}
    and exists $option->{swing_h}
    and defined $option->{swing_h}
    and length $option->{swing_h}
    and not exists SWING_H_VAL->{ $option->{swing_h} }
    and Pod::Usage::pod2usage(
        sprintf(
            qq(Invalid swing_h value: "%s". Use: [%s]),
            $option->{swing_h},
            join( "|", sort keys %{ +SWING_H_VAL } )
        )
    )
  )
  or (
    exists $option->{set}
    and exists $option->{swing_v}
    and defined $option->{swing_v}
    and length $option->{swing_v}
    and not exists SWING_V_VAL->{ $option->{swing_v} }
    and Pod::Usage::pod2usage(
        sprintf(
            qq(Invalid swing_v value: "%s". Use: [%s]),
            $option->{swing_v},
            join( "|", sort keys %{ +SWING_V_VAL } )
        )
    )
  )
  or (
    exists $option->{set}
    and exists $option->{breezeless}
    and defined $option->{breezeless}
    and length $option->{breezeless}
    and $option->{breezeless} !~ /^(on|off)$/
    and Pod::Usage::pod2usage(
        qq(Invalid breezeless value: "$option->{breezeless}". Use: [on|off])
    )
  )
  or (
    exists $option->{set}
    and exists $option->{display}
    and defined $option->{display}
    and length $option->{display}
    and $option->{display} !~ /^(on|off)$/
    and Pod::Usage::pod2usage(
        qq(Invalid display value: "$option->{display}". Use: [on|off])
    )
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

if ( exists $option->{caps} ) {
    print vals( diag( $option->{ip} ), %{$option} );
}

if ( exists $option->{set} ) {
    my %out;

    if ($has_c0_set) {
        my $updated =
          update( $option->{ip}, settings( $option, fetch( $option->{ip} ) ) );
        my $named = settings_val($updated);
        for ( grep { exists $option->{$_} } keys %{ +SETTINGS } ) {
            $out{$_} = $named->{$_} if exists $named->{$_};
        }
    }

    if ($has_prop_set) {
        my %want;
        for my $k (qw[swing_h swing_v breezeless display]) {
            $want{$k} = $option->{$k}
              if exists $option->{$k}
              and defined $option->{$k}
              and length $option->{$k};
        }
        my $props = update_props( $option->{ip}, %want );
        @out{ keys %$props } = values %$props;
    }

    print vals( \%out, %{$option} );
}

if ( exists $option->{get} ) {
    my $named = settings_val( fetch( $option->{ip} ) );
    my $props = eval { fetch_props( $option->{ip} ) } // {};
    my $power = eval { fetch_power( $option->{ip} ) } // {};
    my %all   = ( %$named, %$props, %$power );

    my @c0 = grep { exists $option->{$_} } keys %{ +SETTINGS };
    my @extra;
    for my $k (
        qw[swing_h swing_v breezeless display humidity
          power_w energy_total_kwh energy_current_kwh
          energy_operating_kwh electrify_hours]
      )
    {
        push @extra, $k if exists $option->{$k};
    }

    my %out;
    if ( @c0 || @extra ) {
        $out{$_} = $all{$_} for grep { exists $all{$_} } ( @c0, @extra );
    }
    else {
        %out = %all;
    }

    print vals( \%out, %{$option} );
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
    ) unless exists SETTINGS->{ $option->{exit} }
      && exists SETTINGS->{ $option->{exit} }->{parse};

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
   --caps            query device capabilities (status + B5 capability report)

   --discover        searches for compatible devices

   --power           turn power state: [on|off]
   --temp            set target temperature: [16..30]
   --mode            set operational mode: [auto|cool|dry|heat|fan]
   --fan             set fan speed: [auto|high|medium|low|silent]
                     (mute accepted as alias for silent; output always silent)
   --turbo           turn turbo mode: [on|off]
   --swing           set swing mode: [off|vertical|horizontal|both]
   --swing_h         horizontal louver angle (B0/B1):
                     [off|left|left_mid|middle|right_mid|right]
                     (on this model, off is ignored)
   --swing_v         vertical louver angle (B0/B1):
                     [off|up|up_mid|middle|down_mid|down]
                     (on this model, off is ignored)
   --breezeless      breezeless / no-wind-feel (B0/B1): [on|off]
   --display         panel screen display (B0/B1): [on|off]
   --humidity        indoor humidity from B1 (get only)
   --eco             turn eco mode: [on|off]
   --sleep           turn sleep mode: [on|off]
   --buzzer          turn audible feedback: [on|off]
   --power_saving    turn power saving: [on|off]
   --smart_eye       turn smart eye / follow: [on|off]
   --dry_clean       turn dry clean: [on|off]
   --aux_heat        turn auxiliary PTC heat: [on|off]
   --anion           turn anion / ionizer: [on|off]
   --natural_wind    turn natural wind: [on|off]
   --frost_protect   turn frost protect: [on|off]
   --comfort         turn comfort mode: [on|off]
   --led             turn C0 display LED: [on|off]
   --unit            temperature unit: [C|F]

   --value           output of values alone
   --begin           beginning of the output string [default: none]
   --end             end of the output string [default: "\n"]
   --quote           use quotes [default: none]
   --unquote_num     do not use quotes for numbers
   --separator       field separator [default: ":"]
   --delimiter       fields delimiter [default: "\n"]

   --exit            exit code 0 if value ON, else exit code 1
                     [power|eco|led|error|turbo|sleep|buzzer|unit|
                      power_saving|smart_eye|dry_clean|aux_heat|anion|
                      natural_wind|frost_protect|comfort]

=head1 OPTIONS

=over 4

=item B<--help>

Print a brief help message and exits.

=item B<--ip>

IP address or host name of the device

=item B<--caps>

Query device capabilities. Sends a standard status query (CMD_41), B5
capability query (0xB5, including a second frame when the device reports
more capabilities), B1 property query (louver angles, breezeless, display,
humidity), and group_data power query (0x41/0xC1). Outputs current operating
state alongside device capability flags. Unknown B5 entries appear as
C<cap_XX_XX> with their raw hex values.

Example:

  ac.pl --ip 192.168.1.2 --caps

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

C<mute> is accepted as an input alias for C<silent>. Status output always
uses the canonical name C<silent>.

=item B<--turbo>

The parameter controls the turbo mode of the device

It can take one of the following values: [on|off]

=item B<--swing>

The parameter controls the operation mode of the blinds of the device

It can take one of the following values: [off|vertical|horizontal|both]

=item B<--swing_h>

Fixed horizontal louver angle via B0/B1 property protocol (tag 0x000A).

Values: [off|left|left_mid|middle|right_mid|right]

Complements C<--swing>; use after enabling horizontal swing if needed.

On this model, angle C<off> is ignored (firmware keeps the last fixed
position). Use a named position to move the louvers.

=item B<--swing_v>

Fixed vertical louver angle via B0/B1 property protocol (tag 0x0009).

Values: [off|up|up_mid|middle|down_mid|down]

On this model, angle C<off> is ignored (same as C<--swing_h>).

=item B<--breezeless>

Breezeless / no-wind-feel via B0/B1 property protocol (tag 0x0018).

Values: [on|off]

=item B<--display>

Panel screen display via B0/B1 property protocol (tag 0x0017). Distinct from
C<--led> (C0 SETTINGS bit).

Values: [on|off]

=item B<--humidity>

Indoor relative humidity (%), read-only via B1 property tag 0x0015.
Use with C<--get> (or included in full C<--get> / C<--caps>).

=item B<--eco>

The parameter controls the eco mode of the device

It can take one of the following values: [on|off]

=item B<--sleep>

The parameter controls the sleep mode of the device

It can take one of the following values: [on|off]

=item B<--buzzer>

The parameter controls the sound response mode of the device (prompt tone)

It can take one of the following values: [on|off]

=item B<--power_saving>

Power saving mode. Values: [on|off]

=item B<--smart_eye>

Smart eye / follow-me style sensor. Values: [on|off]

=item B<--dry_clean>

Dry clean mode. Values: [on|off]

=item B<--aux_heat>

Auxiliary PTC heat. Values: [on|off]

=item B<--anion>

Anion / ionizer. Values: [on|off]

=item B<--natural_wind>

Natural wind. Values: [on|off]

=item B<--frost_protect>

Frost protect. Values: [on|off]

=item B<--comfort>

Comfort mode. Values: [on|off]

=item B<--led>

Panel / display LED. Values: [on|off]

=item B<--unit>

Temperature unit. Values: [C|F]

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
