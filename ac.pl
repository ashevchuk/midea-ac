#!/usr/bin/env perl

use strict;
use warnings;

use Pod::Usage   ();
use Getopt::Long ();

use IO::Socket       ();
use List::Util       ();
use Scalar::Util     ();
use Digest::MD5      ();
use Digest::CRC      ();
use Crypt::Mode::ECB ();

use constant {
    PORT    => 6444,
    RETRY   => 3,
    TIMEOUT => 5,
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

use constant { KEY_SIGN => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S' };

use constant { KEY => Digest::MD5::md5(KEY_SIGN) };

use constant {
    SETTINGS => {
        fan => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x0d] = $_[1] }
            },
            parse => sub { List::Util::min( ( $_[0]->[0x03] & 0x7f ), 0x65 ) },
            val => {
                auto   => 0x65,
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
                  sub { $_[0]->[0x11] &= ~0x0f; $_[0]->[0x11] |= $_[1] & 0x0f }
            },
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
            parse => sub { OFF },
            val   => {
                off => OFF,
                on  => ON
            }
        },
        error => {
            parse => sub { ( $_[0]->[0x01] & 0x80 ) > OFF ? ON : OFF },
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
            parse => sub { ( $_[0]->[0x02] & 0x0f ) + BLOCK_LEN },
            val => { map { ( $_, $_ ) } ( TEMP_MIN .. TEMP_MAX ) }
        },
        eco => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x13] = $_[1] ? 0xff : OFF }
            },
            parse => sub { ( $_[0]->[0x09] & 0x10 ) > OFF ? ON : OFF },
            val => {
                off => OFF,
                on  => ON
            }
        },
        turbo => {
            input => {
                type => OPT_STR,
                set  => sub { $_[0]->[0x14] = $_[1] ? 0x02 : OFF }
            },
            parse => sub { ( $_[0]->[0x0a] & 0x02 ) > OFF ? ON : OFF },
            val => {
                off => OFF,
                on  => ON
            }
        },
        temp_int => {
            parse => sub { ( $_[0]->[0x0b] - 0x32 ) / 0x02 },
        },
        temp_ext => {
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

sub inflate ($) {
    return [ map { ord } split //, $_[0] ];
}

sub deflate (+@) {
    return join EMPTY_STR, map { chr } @{ $_[0] };
}

sub encrypt ($) {
    return inflate ( Crypt::Mode::ECB->new( AES => 0 )->encrypt( deflate( $_[0] ), KEY ) );
}

sub decrypt ($) {
    return inflate ( Crypt::Mode::ECB->new( AES => 0 )->decrypt( deflate( $_[0] ) , KEY ) );
}

sub crc8 (+@) {
    return Digest::CRC->new(
        width  => 8,
        init   => 0x00,
        xorout => 0x00,
        refout => 1,
        poly   => 0x0131,
        refin  => 1,
        cont   => 1
    )->add( deflate $_[0] )->digest();
}

sub unescape ($) {
    return $_[0] // EMPTY_STR =~ s{ (\A|\G|[^\\]) [\\] ( [0]\d\d | [x][\da-fA-F]{2} | . ) }{ $1 . ( ESCAPES->{ lc( $2 ) } ) }sgerx;
}

sub quote {
    my ( $value, %opts ) = @_;
    return (exists $opts{unquote_num} and Scalar::Util::looks_like_number( $value ))
        ? $value
        : length($value)
              ? sprintf("%s%s%s", unescape( $opts{quote} ) // EMPTY_STR, $value, unescape( $opts{quote} ) // EMPTY_STR)
              : EMPTY_STR;
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

    push @{$command},
      0xff - List::Util::sum0( @{$command}[ 0x01 .. $#$command ] ) % 0x0100 + 0x01;

    my $pad = BLOCK_LEN - ( scalar( @{$command} ) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt( $command ) };

    $packet->[0x04] = scalar( @{$packet} ) + BLOCK_LEN;

    push @{$packet},
      @{ inflate Digest::MD5::md5( deflate( $packet ) . KEY_SIGN ) };

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
    my $body = [ @{$data}[ 0x0a .. $#$data ] ];
    return { map { exists SETTINGS->{$_}->{parse} ? ( $_, SETTINGS->{$_}->{parse}->($body) ) : () } keys %{ +SETTINGS } };
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

sub net_request {
    my ( $device_ip, $data ) = @_;

    my $client = IO::Socket->new(
        Domain   => IO::Socket::AF_INET,
        Type     => IO::Socket::SOCK_STREAM,
        Proto    => 'tcp',
        Timeout  => TIMEOUT,
        PeerPort => PORT,
        PeerHost => $device_ip
    ) or die "Socket error: $@";

    $client->send( deflate $data) == scalar @{$data}
      or $client->close(), die "Send error";
    $client->recv( my $buffer, RESPONSE_LEN );
    $client->close();

    length($buffer) == RESPONSE_LEN or die "Recv error";

    return inflate $buffer;
}

sub send_request {
    my ( $device_ip, $data ) = @_;
    my $response =
      decrypt(
        [ @{ net_request( $device_ip, $data ) }[ 0x28 .. 0x57 ] ] );
    return [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
}

sub request {
    my ( $device_ip, $packet ) = @_;

    my $last_error;
    my $retry = RETRY;

    while (--$retry) {
        my $data = eval { send_request( $device_ip, $packet ) } or $last_error = $@, next;
        return device_settings($data);
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

my $option = {};

Getopt::Long::GetOptions(
    $option,
    qw[
      help
      set
      get
      value
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

Pod::Usage::pod2usage(1) if exists $option->{help};

Pod::Usage::pod2usage(2)
  if ( not ( exists $option->{exit} ) and not ( exists $option->{set} or exists $option->{get} ) )
  or ( not( exists $option->{ip} ) )
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
          )
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
    Pod::Usage::pod2usage( sprintf(qq(Invaid exit value: "%s". It can take one of the following values: [%s]), $option->{exit}, join "|", grep { exists SETTINGS->{$_}->{parse} and exists SETTINGS->{$_}->{state} and SETTINGS->{$_}->{state} eq "bool" } keys %{ +SETTINGS } ) )
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
