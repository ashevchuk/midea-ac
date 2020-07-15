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
    PORT_DEVICE         => 6444,
    KEY_SIGN            => 'xhdiwjnchekd4d512chdjx5d8e4c394D2D7S',
    DEVICE_BLOCK_LEN    => 16,
    DEVICE_RESPONSE_LEN => 104
};

use constant {
    TEMP_VALUE_MIN      => 17,
    TEMP_VALUE_MAX      => 30,
};

use constant {
    STATUS_ON  => 0x01,
    STATUS_OFF => 0x00
};

my $key = Digest::MD5::md5( KEY_SIGN );

my $ecb = Crypt::Mode::ECB->new( AES => 0 );

my @packet_tmpl = (
    0x5a, 0x5a, 0x01, 0x11, 0x68, 0x00, 0x20, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
);
my @command_tmpl = (
    0xaa, 0x20, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x41, 0x81,
    0x00, 0xff, 0x03, 0xff, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
);

my $enum = {
    fan => {
        input => {
            type => "s",
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
            type => "s",
            set  => sub {
                $_[0]->[0x0c] &= ~0xe0;
                $_[0]->[0x0c] |= ( $_[1] << 5 ) & 0xe0;
              }
        },
        parse => sub { ( $_[0]->[0x02] & 0xe0 ) >> 5 },
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
            type => "s",
            set => sub { $_[0]->[0x11] &= ~0x0f; $_[0]->[0x11] |= $_[1] & 0x0f }
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
            type => "s",
            set =>
              sub { $_[0]->[0x0b] &= ~0x01; $_[0]->[0x0b] |= $_[1] ? 0x01 : 0 }
        },
        parse => sub { ( $_[0]->[0x01] & 0x01 ) > 0 ? STATUS_ON : STATUS_OFF },
        val => {
            off => STATUS_OFF,
            on  => STATUS_ON
        }
    },
    buzzer => {
        input => {
            type => "s",
            set =>
              sub { $_[0]->[0x0b] &= ~0x42; $_[0]->[0x0b] |= $_[1] ? 0x42 : 0 }
        },
        parse => sub { STATUS_OFF },
        val   => {
            off => STATUS_OFF,
            on  => STATUS_ON
        }
    },
    error => {
        parse => sub { ( $_[0]->[0x01] & 0x80 ) > 0 ? STATUS_ON : STATUS_OFF },
        val => {
            no  => STATUS_OFF,
            yes => STATUS_ON
        }
    },
    temp => {
        input => {
            type => "i",
            set  => sub {
                $_[0]->[0x0c] &= ~0x1f;
                $_[0]->[0x0c] |= ( $_[1] & 0x0f ) | ( ( $_[1] << 4 ) & 0x10 );
              }
        },
        parse => sub { ( $_[0]->[0x02] & 0x0f ) + 16 },
        val => { map { ( $_, $_ ) } ( TEMP_VALUE_MIN .. TEMP_VALUE_MAX ) }
    },
    eco => {
        input => {
            type => "s",
            set  => sub { $_[0]->[0x13] = $_[1] ? 0xff : 0 }
        },
        parse => sub { ( $_[0]->[0x09] & 0x10 ) > 0 ? STATUS_ON : STATUS_OFF },
        val => {
            off => STATUS_OFF,
            on  => STATUS_ON
        }
    },
    turbo => {
        input => {
            type => "s",
            set  => sub { $_[0]->[0x14] = $_[1] ? 0x02 : 0 }
        },
        parse => sub { ( $_[0]->[0x0a] & 0x02 ) > 0 ? STATUS_ON : STATUS_OFF },
        val => {
            off => STATUS_OFF,
            on  => STATUS_ON
        }
    },
    temp_int => {
        parse => sub { ( $_[0]->[0x0b] - 50 ) / 2.0 },
    },
    temp_ext => {
        parse => sub { ( $_[0]->[0x0c] - 50 ) / 2.0 },
    },
};

my $enum_val = {
    map {
        my $type = $_;
        (
            $type => {
                val => {
                    map { ( $enum->{$type}->{val}->{$_}, $_ ) } keys %{
                        exists $enum->{$type}->{val}
                        ? $enum->{$type}->{val}
                        : {}
                    }
                }
            }
          )
    } keys %{$enum}
};

sub inflate ($) {
    my ($data) = @_;
    return [ map { ord } split //, $data ];
}

sub deflate (+@) {
    my ($data) = @_;
    return join "", map { chr } @{$data};
}

sub get_cmd {
    my $data = [@command_tmpl];
    push @{$data},
      Digest::CRC->new(
        width  => 8,
        init   => 0x00,
        xorout => 0x00,
        refout => 1,
        poly   => 0x131,
        refin  => 1,
        cont   => 1
      )->add( deflate [ @{$data}[ 10 .. $#$data ] ] )->digest();
    return $data;
}

sub set_cmd {
    my ($settings) = @_;

    my $data = [@command_tmpl];

    $data->[0x01] = 0x23;
    $data->[0x09] = 0x02;
    $data->[0x0a] = 0x40;
    push @{$data}, (0x00) x 3;

    foreach my $key ( keys %{$settings} ) {
        $enum->{$key}->{input}->{set}->( $data, $settings->{$key} )
          if exists $enum->{$key}->{input}->{set};
    }

    push @{$data},
      Digest::CRC->new(
        width  => 8,
        init   => 0x00,
        xorout => 0x00,
        refout => 1,
        poly   => 0x131,
        refin  => 1,
        cont   => 1
      )->add( deflate [ @{$data}[ 10 .. $#$data ] ] )->digest();

    return $data;
}

sub packet {
    my ( $device_id, $command ) = @_;
    my $packet = [@packet_tmpl];

    @{$packet}[ 20, 25 ] =
      @{ inflate reverse( pack( 'H*', sprintf( "%.2x", $device_id ) ) ) };

    push @{$command},
      255 - List::Util::sum0( @{$command}[ 1 .. $#$command ] ) % 256 + 1;

    my $pad = DEVICE_BLOCK_LEN - ( scalar( @{$command} ) % DEVICE_BLOCK_LEN );
    push @{$command}, ($pad) x $pad;

    push @{$packet}, @{ inflate $ecb->encrypt( deflate($command), $key ) };

    $packet->[0x04] = scalar( @{$packet} ) + 16;

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

    foreach my $key ( keys %{$enum} ) {
        if ( exists $enum->{$key}->{parse} ) {
            $response->{$key} = $enum->{$key}->{parse}->($body);
        }
    }

    return $response;
}

sub response_val {
    my ($data) = @_;

    my $response = {};

    my $body = [ @{$data}[ 0x0a .. $#$data ] ];

    foreach my $key ( keys %{$enum} ) {
        if ( exists $enum->{$key}->{parse} ) {
            my $value = $enum->{$key}->{parse}->($body);
            $response->{$key} = $enum_val->{$key}->{val}->{$value} // $value;
        }
    }

    return $response;
}

sub settings {
    my ( $new, $old ) = @_;
    my $combined = {};

    foreach my $key ( keys %{$enum} ) {
        if ( exists $enum->{$key}->{input} ) {
            $combined->{$key} =
              (       exists( $new->{$key} )
                  and exists( $enum->{$key}->{val}->{ $new->{$key} } ) )
              ? $enum->{$key}->{val}->{ $new->{$key} }
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
        Timeout  => 5,
        PeerPort => PORT_DEVICE,
        PeerHost => $device_ip
    ) or die "Socket error: $@";

    $client->send( deflate $data) == scalar @{$data}
      or $client->close(), die "Send error";
    $client->recv( my $buffer, DEVICE_RESPONSE_LEN );
    $client->close();

    length($buffer) == DEVICE_RESPONSE_LEN or die "Recv error";

    return inflate $buffer;
}

sub request {
    my ( $device_ip, $data ) = @_;
    my $response =
      inflate $ecb->decrypt(
        deflate( [ @{ send_request( $device_ip, $data ) }[ 40 .. 87 ] ] ),
        $key );
    return [ @{$response}[ 0 .. $#$response - $response->[-1] ] ];
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
    $option, qw[
      help
      set
      get
      id=i
      ip=s
      ],
    map { join "=", ( $_, $enum->{$_}->{input}->{type} ) }
      grep { exists $enum->{$_}->{input} } keys %{$enum}
);

Pod::Usage::pod2usage(1) if exists $option->{help};

Pod::Usage::pod2usage(2)
  if ( not( exists $option->{set} or exists $option->{get} ) )
  or ( not( exists $option->{id} and exists $option->{ip} ) )
  or (
    grep {
        my $item = $_;
        (
                  exists( $option->{$item} )
              and exists( $enum->{$item} )
              and ( not scalar grep { lc $option->{$item} eq $_ }
                keys %{ $enum->{$item}->{val} } )
          )
    } grep { exists $enum->{$_}->{input} } keys %{$enum}
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
