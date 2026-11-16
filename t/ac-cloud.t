#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);

# ac-cloud.pl is a script, not a module, but its CLI/Getopt/network
# driver is guarded by `unless (caller)` precisely so this test can
# `require` it and call its packet-building subs directly, no network
# or real V3 hardware involved — same pattern as t/ac_ng.t.
require "$Bin/../ac-cloud.pl";

# --- crc8(): same CRC-8/854 table as ac.pl/ac-ng.pl's own crc8() ---

is( crc8( [ 0x41, 0x81, 0x00, 0xff, (0x00) x 16, 1 ] ), 0xab,
    'crc8: matches the independently hand-computed value for the StateQuery body' );

# --- v2_status_packet(): byte-exact against a worked example derived
# from midealan/devices/ac/message.py (StateQuery) and message.py
# (MessageRequest), with device protocol version = 0 and message ID = 1
# (this function's fixed default — a one-shot process, not a session) ---

{
    my $captured_inner;
    no warnings qw(once redefine);
    local *main::aes_ecb = sub {
        my ( undef, undef, $data ) = @_;
        $captured_inner = $data;
        return $data;    # skip real padding/encryption, just observe the plaintext
    };
    local *main::md5_bin = sub { return "\x00" x 16 };

    v2_status_packet();

    is( unpack( 'H*', $captured_inner ),
        'aa20ac00000000000003418100ff0000000000000000000000000000000001abc4',
        'v2_status_packet: inner AA-frame matches the midealan-derived worked example byte for byte'
    );
}

# --- encode_8370(): size-field/padding math against a worked example
# (10-byte payload -> padding 4, size field 0x2E, header byte 5 = 0x46,
# total frame 54 bytes) — this is exactly the case the old code got
# wrong when (payload_len+2) was NOT already block-aligned, so it's a
# real regression guard, not just a restatement of the fix. ---

{
    no warnings qw(once redefine);
    local $main::REQ_COUNT = 0;
    local $main::TCP_KEY   = "\x11" x 32;
    local *main::aes_cbc = sub { return $_[2] };    # identity: observe plaintext length only

    my $pkt = encode_8370( 'x' x 10, &main::MSG_ENC_REQUEST );
    is( length($pkt), 54, 'encode_8370: total frame length matches the worked example' );
    my ( $m0, $m1, $size, $b4, $b5 ) = unpack( 'CCnCC', substr( $pkt, 0, 6 ) );
    is( $size, 0x2e, 'encode_8370: size field matches the worked example (padding was needed)' );
    is( $b5,   0x46, 'encode_8370: header byte 5 (padding nibble + msgtype) matches the worked example' );

    # Regression guard for the actual bug: a payload where (len+2) is
    # ALREADY block-aligned needs no padding, but the size field must
    # still gain +32 for the sign — the old code only added +32 inside
    # the "padding needed" branch, so this case came out as 14, not 46.
    my $pkt2 = encode_8370( 'x' x 14, &main::MSG_ENC_REQUEST );
    my ( undef, undef, $size2, undef, $b5_2 ) = unpack( 'CCnCC', substr( $pkt2, 0, 6 ) );
    is( $size2, 46, 'encode_8370: size field still gains +32 when no padding is needed' );
    is( $b5_2 & 0xf0, 0, 'encode_8370: padding nibble is 0 when already block-aligned' );
}

done_testing;
