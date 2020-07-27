#!/usr/bin/env perl

use strict;
use warnings;

use constant {
    PORT    => 6444,
    RETRY   => 5,
    TIMEOUT => 8,
};

use constant { KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S' };

use constant {
    PACKET => [
        0x5a, 0x5a, 0x01, 0x11, 0x68, 0x00, 0x20, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ],
    COMMAND => [
        0xaa, 0x20, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x03, 0x41, 0x81, 0x00, 0xff, 0x03, 0xff,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
};

use constant {
    BLOCK_LEN    => 16,
    RESPONSE_LEN => 104
};

use constant {
    TEMP_MIN => 17,
    TEMP_MAX => 30
};

use constant {
    OFF => 0x00,
    ON  => 0x01
};

use constant {
    OPT_STR => "s",
    OPT_INT => "i"
};

use constant {
    DELIMITER => "\n",
    SEPARATOR => ":",
    EMPTY_STR => "",
    OUT_BEGIN => undef,
    OUT_END   => "\n",
    ESCAPES   => {
        ( map { $_ => $_ } ( '\\', '"', '$', '@' ) ),
        ( 'r' => "\r", 'n' => "\n", 't' => "\t" ),
        ( map { 'x' . unpack( 'H2', chr( $_ ) ) => chr( $_ ) } ( 0..255 ) ),
        ( map { sprintf( '%03o', $_ ) => chr( $_ ) } ( 0..255 ) )
    }
};

use constant {
    SETTINGS => {
        fan => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x0d] = $_[1] }
            },
            state => 'val',
            parse => sub { ( $_[0]->[0x03] & 0x7f ) },
            val => {
                auto   => 0x66,
                high   => 0x50,
                medium => 0x3c,
                low    => 0x28,
                silent => 0x14
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
            state => 'val',
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
                set =>
#                  sub { $_[0]->[0x11] &= ~0x0f; $_[0]->[0x11] |= $_[1] & 0x0f }
                  sub { $_[0]->[0x11] &= ~0x30; $_[0]->[0x11] |= $_[1] & 0x3f }
            },
            state => 'val',
#            parse => sub { $_[0]->[0x07] & 0x0f },
            parse => sub { $_[0]->[0x11] },
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
            state => 'bool',
            parse => sub { ( $_[0]->[0x01] & 0x01 ) > OFF ? ON : OFF },
            val => {
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
            state => 'bool',
            parse => sub { ( $_[0]->[0x0b] & 0x42 ) > OFF ? ON : OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        error => {
            parse => sub { ( $_[0]->[0x01] & 0x80 ) > OFF ? ON : OFF },
            state => 'bool',
            val => {
                no  => OFF,
                yes => ON
            }
        },
        temp => {
            input => {
                type => OPT_INT,
                set  => sub {
                    $_[0]->[0x0c] &= ~0x1f;
                    $_[0]->[0x0c] |=
                      ( $_[1] & 0x0f ) | ( ( $_[1] << 0x04 ) & 0x10 );
                  }
            },
            state => 'val',
            parse => sub { ( $_[0]->[0x02] & 0x0f ) + 16 },
            val => { map { ( $_, $_ ) } ( TEMP_MIN .. TEMP_MAX ) }
        },
        eco => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x13] = $_[1] ? 0xff : OFF }
            },
            state => 'bool',
            parse => sub { ( $_[0]->[0x09] & 0x10 ) > OFF ? ON : OFF },
            val => {
                off => OFF,
                on  => ON
            }
        },
        turbo => {
            input => {
                type => OPT_STR,
#                set  => sub { $_[0]->[0x14] = $_[1] ? 0x02 : OFF }
                set  => sub { $_[1] ? $_[0]->[0x14] |= 0x02 : $_[0]->[0x14] &= ( ~ 0x02 ) }
            },
            state => 'bool',
#            parse => sub { ( $_[0]->[0x0a] & 0x02 ) > OFF ? ON : OFF },
            parse => sub { $_[0]->[0x14] > OFF ? ON : OFF },
            val => {
                off => OFF,
                on  => ON
            }
        },
        led => {
            input => {
                type => OPT_STR,
                set  => sub { $_[1] ? $_[0]->[0x14] |= 0x10 : $_[0]->[0x14] &= ( ~ 0x10 ) }
            },
            state => 'bool',
#            parse => sub { ( $_[0]->[0x14] & 0x10 ) > OFF ? ON : OFF },
            parse => sub { ( $_[0]->[0x0a] & 0x10 ) > OFF ? ON : OFF },
            val => {
                off => OFF,
                on  => ON
            }
        },
        unit => {
            input => {
                type => OPT_STR,
                set  => sub { $_[1] ? $_[0]->[0x14] |= 0x04 : $_[0]->[0x14] &= ( ~ 0x04 ) }
            },
            state => 'bool',
#            parse => sub { ( $_[0]->[0x14] & 0x04 ) > OFF ? ON : OFF },
            parse => sub { ( $_[0]->[0x09] & 0x80 ) > OFF ? ON : OFF },
            val => {
                "C" => OFF,
                "F"  => ON
            }
        },
        temp_int => {
            state => 'val',
            parse => sub { ( $_[0]->[0x0b] - 0x32 ) / 0x02 },
        },
        temp_ext => {
            state => 'val',
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
        do { $p |= 0x01 << ( $_[0] - $_ ) if $v & 0x01; $v = $v >> 0x01; } for 0x01..$_[0];
        return map { my $i = $_; $i = ( $i >> 0x01 ) ^ ( $i & 0x01 && $p ) for 0x00..0x07; $i & 0x02 ** $_[0] - 0x01 } 0x00..0xff;
    }
};

use constant {
    CRC8_TABLE => [ CRC8_TABLE_GEN->(8, 0x0131) ]
};

sub ahex {
    my ($item) = @_;
    if (ref ($item) eq "ARRAY") {
        return ( join "", map { sprintf("%.2x", $_) } @{$item} );
    } elsif (ref ($item) eq "") {
        return unpack( "H*", $item);
    }

}

sub _hex {
    my ($item) = @_;
    if (ref ($item) eq "") {
        return unpack( "H*", $item);
    } elsif (ref ($item) eq "ARRAY") {
        return "[" . ( join ",", map { sprintf("0x%.2x", $_) } @{$item} ) . "]";
    } else {
        return "$item";
    }
}

sub debug {
    foreach my $item (@_) {
        if (ref ($item) eq "") {
            printf("%s", $item);
        } elsif (ref ($item) eq "ARRAY") {
            printf("[%s]", (join (",", map { sprintf("0x%.2x", $_) } @{$item})));
        } elsif (ref ($item) eq "HASH") {
            printf("{%s}", join ",", (map { sprintf("%s:%s", $_, eval{$item->{$_} == int($item->{$_})} ? sprintf("0x%.2x", $item->{$_}) : $item->{$_}) } sort keys %{$item}));
        } else {
            print("$item");
        }
    }
    print("\n");
}

sub crc8 (+@) {
    my $crc = 0;
    $crc = CRC8_TABLE->[$crc ^ $_] for @{ $_[0] };
    return $crc;
}

sub list_sum (@) {
    my $sum = 0;
    $sum += $_ for @_;
    return $sum;
}

sub inflate ($) {
    return [ map { ord } split //, $_[0] ];
}

sub deflate (+@) {
    return join EMPTY_STR, map { defined ? chr : 0x00 } @{ $_[0] };
}

sub encrypt ($) {
    my $cmd = join " ", qw(echo -n), ahex( $_[0] ), qw(| xxd -r -p | openssl enc -e -nopad -aes-128-ecb -K), ahex(md5(KEY_SIGN)), qw(-in - -out - | xxd -p);
    return inflate pack("H*",  join "", split /\n/, qx($cmd));
}

sub decrypt ($) {
    my $cmd = join " ", qw(echo -n), ahex( $_[0] ), qw(| xxd -r -p | openssl enc -d -nopad -aes-128-ecb -K), ahex(md5(KEY_SIGN)), qw(-in - -out - | xxd -p);
    return inflate pack("H*",  join "", split /\n/, qx($cmd));
}

sub md5 ($) {
    my $cmd = join " ", qw(echo -n), ahex( $_[0] ), qw(| xxd -r -p | md5sum);
    return inflate pack("H*",  (split /\s/, qx($cmd))[0]);
}

sub net_request ($$) {
    my $cmd = join " ", qw(echo -n), ahex( $_[1] ), qw(| xxd -r -p | nc -n -I), RESPONSE_LEN, qw(-w), TIMEOUT, $_[0], PORT, qw(| xxd -p);
    return inflate pack("H*",  join "", split /\n/, qx($cmd));
}

sub unescape ($) {
    return $_[0] // EMPTY_STR =~ s{ (\A|\G|[^\\]) [\\] ( [0]\d\d | [x][\da-fA-F]{2} | . ) }{ $1 . ( ESCAPES->{ lc( $2 ) } ) }sgerx;
}

sub is_number ($) {
    $_[0] =~ /^[0-9\.]+$/;
}

sub quote {
    my ( $value, %opts ) = @_;
    if (exists $opts{unquote_num} and is_number( $value )) {
        return $value;
     } else {
        if ( length($value) ) {
            return sprintf("%s%s%s", unescape( $opts{quote} ) // EMPTY_STR, $value, unescape( $opts{quote} ) // EMPTY_STR);
        } else {
            return EMPTY_STR;
        }
    }
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

    for( keys %{$settings} ) {
        SETTINGS->{$_}->{input}->{set}->( $data, $settings->{$_} )
          if exists SETTINGS->{$_}->{input}->{set};
    }

    push @{$data}, crc8( [ @{$data}[ 0x0a .. $#$data ] ] );

    return $data;
}


sub packet {
    my ( $command ) = @_;
    my $packet = [ @{ +PACKET } ];

    push @{$command}, ( ( ~ list_sum( @{$command}[ 0x01 .. $#$command ] ) + 0x01 ) & 0xff );

    my $pad = BLOCK_LEN - ( scalar( @{$command} ) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt( $command ) };

    $packet->[0x04] = scalar( @{$packet} ) + BLOCK_LEN;

    push @{$packet},
      @{ md5( deflate( $packet ) . KEY_SIGN ) };

    return $packet;
}

sub set_packet {
    return packet( set_cmd( $_[0] ) );
}

sub status_packet {
    return packet( get_cmd() );
}

sub device_settings {
    my ($data) = @_;

    die "unknown response: " . hexa( $data ) unless defined $data->[0x0a] and $data->[0x0a] == 0xc0;

    my $body = [ @{$data}[ 0x0a .. $#$data ] ];

    return {
        map {
            (exists SETTINGS->{$_}->{parse})
            ? do {
                my $value = SETTINGS->{$_}->{parse}->($body);
                die "unknown \"$_\" value: \"$value\"" if scalar keys %{ SETTINGS_VAL->{$_}->{val} } and not exists SETTINGS_VAL->{$_}->{val}->{$value};
                ( $_, $value )
            }
            : ()
        } keys %{ +SETTINGS }
    };
}

sub settings_val {
    my ($data) = @_;
    return { map { ( $_, SETTINGS_VAL->{$_}->{val}->{$data->{$_}} // $data->{$_} ) } keys %{ $data } };
}

sub settings_str {
    my ( $data, $fields, %opts ) = @_;

    my $settings = settings_val($data);

    return join EMPTY_STR,
        exists $opts{begin} ? unescape( $opts{begin} ) : OUT_BEGIN // EMPTY_STR,
        (join exists $opts{delimiter} ? unescape( $opts{delimiter} ) : DELIMITER,
            map { sprintf( "%s%s%s",
                    quote( $opts{value} ? EMPTY_STR : $_, %opts ),
                    exists $opts{separator} ? unescape( $opts{separator} ) : ( exists $opts{value} ? EMPTY_STR : SEPARATOR ),
                    quote( $settings->{$_}, %opts )
            ) }
            sort grep { my $field = $_; scalar @{ $fields } ? grep { $field eq $_ } @{ $fields } : $field } keys %{$settings}),
        exists $opts{end} ? unescape( $opts{end} ) : OUT_END // EMPTY_STR;
}

sub settings {
    my ( $new, $old ) = @_;

    return { map { ( $_ , ( (
        exists( $new->{$_} ) and exists( SETTINGS->{$_}->{val}->{ $new->{$_} } ) )
          ? SETTINGS->{$_}->{val}->{ $new->{$_} }
          : $old->{$_} ) )
        } grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS } };
}

sub send_request {
    my ( $device_ip, $data ) = @_;
    my $response = net_request( $device_ip, $data );
    $response = decrypt( [ @{$response}[ 0x28 .. ( ( scalar @{ $response } == 0x58 ) ? 0x47 : 0x57 ) ] ] );
    debug("Decrypted response:", $response);
#        [ @{ net_request( $device_ip, $data ) }[ 0x28 .. 0x57 ] ] );
    return [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
}

sub request {
    my ( $device_ip, $packet ) = @_;

    my $last_error;
    my $retry = RETRY;

    while (--$retry) {
        my $data = eval { device_settings( send_request( $device_ip, $packet ) ) } or $last_error = $@, next;
        return $data;
    }

    die $last_error;
}

sub update {
    my ( $device_ip, $settings ) = @_;
    return request( $device_ip, set_packet( $settings ) );
}

sub fetch {
    my ( $device_ip ) = @_;
    return request( $device_ip, status_packet() );
}

sub usage {
    my ( $data ) = @_;
    my $state = {};
    my $doc = {};
    for (<DATA>) {
        chomp;
        next unless length;
        s{[A-Z]\<(.*?)\>}{$1}sg;
        if (/^\=([a-z]+)\d*?\s(.*?)$/) {
            $state->{mode} = $1;
            $state->{$state->{mode}} = $2;
        } elsif (/^\=([a-z])+$/) {
            $state->{mode} = $1;
        } else {
            if ($state->{mode} eq "head") {
                push @{$doc->{$state->{$state->{mode}}} //= []}, $_;
            } elsif ($state->{mode} eq "item") {
                push @{$doc->{$state->{head}}->{$state->{$state->{mode}}} //= []}, $_;
            }
        }
    }

    printf("Error: %s\n", $data) if $data;

    foreach my $head (($data ? () : qw(NAME DESCRIPTION)), qw(SYNOPSIS), ($data ? () : qw(OPTIONS))) {
        printf("%s:\n", $head);
        if (ref $doc->{$head} eq "ARRAY") {
            printf("\t%s\n", $_) for @{ $doc->{$head} };
        } elsif (ref $doc->{$head} eq "HASH") {
            foreach my $item ( keys %{ $doc->{$head} } ) {
                printf("\t%s\n", $item);
                printf("\t\t%s\n", $_) for @{ $doc->{$head}->{$item} };
            }
        }
    }

    exit 1;
}

sub get_options {
    my $options = {};
    my $spec = {};
    for ( @_ ) {
        if (/^(.*?)\=(.*?)$/) {
            $spec->{$1} = { type => $2, req => 1 };
        } elsif (/^(.*?)\:(.*?)$/) {
            $spec->{$1} = { type => $2 };
        } else {
            $spec->{$_} = { type => 'switch' };
        }
    }

    my $state = {};
    for ( @ARGV ) {
        if (/^\-{1,2}([a-z]+)\=(.*?)$/) {
            if (exists $state->{opt}) {
                if(exists $spec->{$state->{opt}}->{req} and not exists $options->{$state->{opt}}) {
                    die "required value: " . $state->{opt};
                } else {
                    delete $state->{opt} if exists $state->{opt};
                }
            }
            $options->{$1} = $2;
            delete $state->{opt} if exists $state->{opt};
        } elsif (/^\-{1,2}([a-z]+)$/) {
            if (exists $state->{opt}) {
                if(exists $spec->{$state->{opt}}->{req} and not exists $options->{$state->{opt}}) {
                    die "required value: " . $state->{opt};
                } else {
                    delete $state->{opt} if exists $state->{opt};
                }
            }
            if ($spec->{$1}->{type} eq "switch") {
                $options->{$1} = undef;
                delete $state->{opt} if exists $state->{opt};
            } else {
                $state->{opt} = $1;
            }
        } elsif (exists $state->{opt}) {
            $options->{$state->{opt}} = $_;
            delete $state->{opt} if exists $state->{opt};
        } else {
            die "invalid option: " . $_;
        }
    }

    return $options;
}

my $option = get_options(
    qw[
      help
      set
      get
      value
      debug
      unquote_num
      ip=s
      delimiter=s
      separator=s
      quote=s
      begin=s
      end=s
      exit=s
    ],
    map { join ":", ( $_, SETTINGS->{$_}->{input}->{type} ) }
      grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
);

unless ( exists $option->{debug} ) {
    no warnings 'redefine';
    *debug = sub { };
}

usage() if exists $option->{help};

usage()
  if ( not( exists $option->{ip} ) )
  or ( not ( exists $option->{exit} ) and not( exists $option->{set} or exists $option->{get} ) )
  or ( exists $option->{set} and not grep { exists SETTINGS->{$_}->{input} } keys %{ $option } )
  or (
    exists $option->{set}
    and grep {
        my $item = $_;
        (
            exists( $option->{$item} ) and exists( SETTINGS->{$item} )
              and (
                not scalar grep { lc $option->{$item} eq $_ }
                keys %{ SETTINGS->{$item}->{val} }
              )
          ) and usage( sprintf(qq(Invaid %s value: "%s". It can take one of the following values: [%s]), $item, $option->{$item}, join "|", keys %{ SETTINGS->{$item}->{val} } ) )
    } grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
  );

if ( exists $option->{set} ) {
    print settings_str( update( $option->{ip},
        settings( $option, fetch( $option->{ip} ) ) ),
        [ grep { exists SETTINGS->{$_} } keys %{ $option } ], %{ $option } );
}

if ( exists $option->{get} ) {
    print settings_str( fetch( $option->{ip} ),
        [ grep { exists SETTINGS->{$_} } keys %{ $option } ], %{ $option } );
}

if ( exists $option->{exit} ) {
    usage( sprintf(qq(Invaid exit value: "%s". It can take one of the following values: [%s]), $option->{exit}, join "|", grep { exists SETTINGS->{$_}->{parse} and exists SETTINGS->{$_}->{state} and SETTINGS->{$_}->{state} eq "bool" } keys %{ +SETTINGS } ) )
            unless exists SETTINGS->{$option->{exit}}->{parse};

    exit ( fetch( $option->{ip} )->{ $option->{exit} } == ON ? 0 : 1 );
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

   --power           turn power state: [on|off]
   --temp            set target temperature: [17..30]
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

   --exit            exit code 0 if value ON, else exit code 1 [eco|led|error|turbo|buzzer|unit|power]

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

It can take positive integer values from this range: [17..30]

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
