#!/usr/bin/env perl

use strict;
use warnings;

use Pod::Usage   ();
use Getopt::Long ();

use IO::Socket       ();
use List::Util       ();
use Digest::MD5      ();
use Digest::CRC      ();
use Crypt::Mode::ECB ();

use constant {
    PORT    => 6444,
    TIMEOUT => 5
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
    PACKET => [
        0x5a, 0x5a, 0x01, 0x11, 0x68, 0x00, 0x20, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
};

use constant {
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
    [ map { ord } split //, $_[0] ];
}

sub deflate (+@) {
    join "", map { chr } @{ $_[0] };
}

sub encrypt ($) {
    inflate ( Crypt::Mode::ECB->new( AES => 0 )->encrypt( deflate( $_[0] ), KEY ) );
}

sub decrypt ($) {
    inflate ( Crypt::Mode::ECB->new( AES => 0 )->decrypt( deflate( $_[0] ) , KEY ) );
}

sub get_cmd {
    my $data = [ @{ +COMMAND } ];
    push @{$data},
      Digest::CRC->new(
        width  => 8,
        init   => 0x00,
        xorout => 0x00,
        refout => 1,
        poly   => 0x0131,
        refin  => 1,
        cont   => 1
      )->add( deflate [ @{$data}[ 0x0a .. $#$data ] ] )->digest();
    return $data;
}

sub set_cmd {
    my ($settings) = @_;

    my $data = [ @{ +COMMAND } ];

    $data->[0x01] = 0x23;
    $data->[0x09] = 0x02;
    $data->[0x0a] = 0x40;
    push @{$data}, (0x00) x 3;

    foreach my $key ( keys %{$settings} ) {
        SETTINGS->{$key}->{input}->{set}->( $data, $settings->{$key} )
          if exists SETTINGS->{$key}->{input}->{set};
    }

    push @{$data},
      Digest::CRC->new(
        width  => 8,
        init   => 0x00,
        xorout => 0x00,
        refout => 1,
        poly   => 0x0131,
        refin  => 1,
        cont   => 1
      )->add( deflate [ @{$data}[ 0x0a .. $#$data ] ] )->digest();

    return $data;
}

sub packet {
    my ( $device_id, $command ) = @_;
    my $packet = [ @{ +PACKET } ];

    @{$packet}[ 0x14, 0x19 ] =
      @{ inflate reverse( pack( 'H*', sprintf( "%.2x", $device_id ) ) ) };

    push @{$command},
      0xff - List::Util::sum0( @{$command}[ 0x01 .. $#$command ] ) % 0x0100 + 0x01;

    my $pad = BLOCK_LEN - ( scalar( @{$command} ) % BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ encrypt( $command ) };

    $packet->[0x04] = scalar( @{$packet} ) + BLOCK_LEN;

    push @{$packet},
      @{ inflate Digest::MD5::md5( deflate($packet) . KEY_SIGN ) };

    return $packet;
}

sub set_packet {
    my ( $device_id, $settings ) = @_;
    packet( $device_id, set_cmd($settings) );
}

sub status_packet {
    my ($device_id) = @_;
    packet( $device_id, get_cmd() );
}

sub device_settings {
    my ($data) = @_;

    my $response = {};

    my $body = [ @{$data}[ 0x0a .. $#$data ] ];

    foreach my $key ( keys %{ +SETTINGS } ) {
        if ( exists SETTINGS->{$key}->{parse} ) {
            $response->{$key} = SETTINGS->{$key}->{parse}->($body);
        }
    }

    return $response;
}

sub response_val {
    my ($data) = @_;

    my $response = {};

    my $body = [ @{$data}[ 0x0a .. $#$data ] ];

    foreach my $key ( keys %{ +SETTINGS } ) {
        if ( exists SETTINGS->{$key}->{parse} ) {
            my $value = SETTINGS->{$key}->{parse}->($body);
            $response->{$key} = SETTINGS_VAL->{$key}->{val}->{$value} // $value;
        }
    }

    return $response;
}

sub settings {
    my ( $new, $old ) = @_;
    my $combined = {};

    foreach my $key ( keys %{ +SETTINGS } ) {
        if ( exists SETTINGS->{$key}->{input} ) {
            $combined->{$key} =
              (       exists( $new->{$key} )
                  and exists( SETTINGS->{$key}->{val}->{ $new->{$key} } ) )
              ? SETTINGS->{$key}->{val}->{ $new->{$key} }
              : $old->{$key};
        }
    }

    return $combined;
}

sub send_request {
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

sub request {
    my ( $device_ip, $data ) = @_;
    my $response =
      decrypt(
        [ @{ send_request( $device_ip, $data ) }[ 0x28 .. 0x57 ] ] );
    return [ @{$response}[ 0x00 .. $#$response - $response->[-1] ] ];
}

sub update {
    my ( $device_ip, $device_id, $settings ) = @_;

    my $data = request( $device_ip, set_packet( $device_id, $settings ) );

    my $response = response_val($data);

    printf(
        "Device response after update:\n%s\n",
        join "\n",
        (
            map { sprintf( "%s:%s", $_, $response->{$_} ) }
            sort keys %{$response}
        )
    );

    return device_settings($data);
}

sub fetch {
    my ( $device_ip, $device_id ) = @_;

    my $data = request( $device_ip, status_packet($device_id) );

    my $response = response_val($data);
    printf(
        "Device response after fetch:\n%s\n",
        join "\n",
        (
            map { sprintf( "%s:%s", $_, $response->{$_} ) }
            sort keys %{$response}
        )
    );

    return device_settings($data);
}

my $option = {};

Getopt::Long::GetOptions(
    $option,
    qw[
      help
      set
      get
      id=i
      ip=s
    ],
    map { join "=", ( $_, SETTINGS->{$_}->{input}->{type} ) }
      grep { exists SETTINGS->{$_}->{input} } keys %{ +SETTINGS }
);

Pod::Usage::pod2usage(1) if exists $option->{help};

Pod::Usage::pod2usage(2)
  if ( not( exists $option->{set} or exists $option->{get} ) )
  or ( not( exists $option->{id} and exists $option->{ip} ) )
  or (
    grep {
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
    update( $option->{ip}, $option->{id},
        settings( $option, fetch( $option->{ip}, $option->{id} ) ) );
}

if ( exists $option->{get} ) {
    fetch( $option->{ip}, $option->{id} );
}

__END__

=head1 NAME

ac.pl - midea air conditioning smart network device remote control (without m-smart cloud)

=head1 SYNOPSIS

ac.pl --id 21354673567853 --ip 192.168.1.2 --set --power on --mode cool --temp 20

 Options:
   --help            brief help message

   --id              device id.  ex: 21354673567853
   --ip              device address. ip address or host name

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

=head1 OPTIONS

=over 4

=item B<--help>

Print a brief help message and exits.

=back

=head1 DESCRIPTION

B<This program> allows you to control your midea air conditioning smart network device without an m-smart cloud.
Potentially, supports devices of other brands.

=cut
