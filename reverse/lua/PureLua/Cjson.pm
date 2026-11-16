package PureLua::Cjson;

use strict;
use warnings;

use PureLua::Table;

# Native Perl port of this project's own reverse/lua/cjson.lua, exposed
# as the engine's `require "cjson"` module. Operates directly on this
# engine's own value representations (PureLua::Table, the Boolean
# singletons via $eval->lua_bool) rather than re-interpreting the Lua
# source - see .notes/2026-11-16-pure-lua-engine-progress.md for why
# self-hosting that file was rejected (it needs pattern/metatable
# features well outside this engine's documented scope, just to satisfy
# a helper file this project wrote, not the real device-script corpus).

sub encode {
    my ($eval, $v) = @_;
    return _encode_value($eval, $v);
}

sub _encode_value {
    my ($eval, $v) = @_;
    my $ty = $eval->_typename($v);
    return 'null'                    if $ty eq 'nil';
    return $$v ? 'true' : 'false'    if $ty eq 'boolean';
    return $eval->_tostring($v)      if $ty eq 'number';
    return _encode_string($$v)       if $ty eq 'string';
    return _encode_table($eval, $v)  if $ty eq 'table';
    die "PureLua::Cjson: cannot encode a $ty value\n";
}

sub _encode_string {
    my ($s) = @_;
    my $out = $s;
    $out =~ s/([\\"])/\\$1/g;
    $out =~ s/\n/\\n/g;
    $out =~ s/\r/\\r/g;
    $out =~ s/\t/\\t/g;
    $out =~ s/([\x00-\x1f])/sprintf('\\u%04x', ord($1))/ge;
    return '"' . $out . '"';
}

# Array-vs-object detection mirrors cjson.lua's own `is_array`: count all
# entries, then check 1..n are every one of them - a table with extra
# non-sequential keys fails that check and becomes an object instead.
sub _encode_table {
    my ($eval, $t) = @_;
    my $list = $t->pairs_list;
    my $n    = scalar @$list;

    my $is_array = 1;
    for my $i (1 .. $n) {
        if (!defined $t->get($i)) { $is_array = 0; last; }
    }

    if ($is_array) {
        return '[' . join( ',', map { _encode_value($eval, $t->get($_)) } 1 .. $n ) . ']';
    }
    return '{' . join( ',', map {
        _encode_string($eval->_tostring($_->[0])) . ':' . _encode_value($eval, $_->[1])
    } @$list ) . '}';
}

sub decode {
    my ($eval, $json) = @_;
    my ($value) = _parse_value($eval, $json, 0);
    return $value;
}

sub _skip_ws {
    my ($s, $i) = @_;
    $i++ while $i < length($s) && substr($s, $i, 1) =~ /\s/;
    return $i;
}

sub _parse_value {
    my ($eval, $s, $i) = @_;
    $i = _skip_ws($s, $i);
    my $c = substr($s, $i, 1);
    if ($c eq '"') {
        my ($text, $j) = _parse_string($s, $i);
        return ( $eval->lua_str($text), $j );
    }
    return _parse_object($eval, $s, $i) if $c eq '{';
    return _parse_array($eval, $s, $i)  if $c eq '[';
    return ( $eval->lua_bool(1), $i + 4 ) if substr($s, $i, 4) eq 'true';
    return ( $eval->lua_bool(0), $i + 5 ) if substr($s, $i, 5) eq 'false';
    return ( undef, $i + 4 )              if substr($s, $i, 4) eq 'null';
    return _parse_number($s, $i);
}

my %ESCAPE_MAP = ( n => "\n", r => "\r", t => "\t", b => "\b", f => "\f", '"' => '"', '\\' => '\\', '/' => '/' );

sub _parse_string {
    my ($s, $i) = @_;
    die "PureLua::Cjson: expected a string at position $i\n" unless substr($s, $i, 1) eq '"';
    my $j   = $i + 1;
    my $out = '';
    while (1) {
        die "PureLua::Cjson: unterminated string\n" if $j >= length($s);
        my $c = substr($s, $j, 1);
        if ($c eq '"') { return ( $out, $j + 1 ); }
        if ($c eq '\\') {
            my $e = substr($s, $j + 1, 1);
            if (exists $ESCAPE_MAP{$e}) { $out .= $ESCAPE_MAP{$e}; $j += 2; }
            elsif ($e eq 'u') {
                my $hex = substr($s, $j + 2, 4);
                $out .= chr( hex($hex) & 0xFF ); # byte-level, matching cjson.lua's own `% 256`
                $j += 6;
            }
            else { $out .= $e; $j += 2; }
            next;
        }
        $out .= $c;
        $j++;
    }
}

sub _parse_number {
    my ($s, $i) = @_;
    substr($s, $i) =~ /\A-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/
        or die "PureLua::Cjson: malformed number at position $i\n";
    my $num = $&;
    return ( $num + 0, $i + length($num) );
}

sub _parse_array {
    my ($eval, $s, $i) = @_;
    my $t = PureLua::Table->new;
    $i = _skip_ws($s, $i + 1);
    return ( $t, $i + 1 ) if substr($s, $i, 1) eq ']';

    my $idx = 1;
    while (1) {
        my ($v, $j) = _parse_value($eval, $s, $i);
        $t->set($idx++, $v);
        $i = _skip_ws($s, $j);
        my $c = substr($s, $i, 1);
        if ($c eq ',') { $i = _skip_ws($s, $i + 1); next; }
        return ( $t, $i + 1 ) if $c eq ']';
        die "PureLua::Cjson: expected ',' or ']' at position $i\n";
    }
}

sub _parse_object {
    my ($eval, $s, $i) = @_;
    my $t = PureLua::Table->new;
    $i = _skip_ws($s, $i + 1);
    return ( $t, $i + 1 ) if substr($s, $i, 1) eq '}';

    while (1) {
        my ($k, $j) = _parse_string($s, $i);
        $i = _skip_ws($s, $j);
        die "PureLua::Cjson: expected ':' at position $i\n" unless substr($s, $i, 1) eq ':';
        my ($v, $j2) = _parse_value($eval, $s, _skip_ws($s, $i + 1));
        $t->set($eval->lua_str($k), $v);
        $i = _skip_ws($s, $j2);
        my $c = substr($s, $i, 1);
        if ($c eq ',') { $i = _skip_ws($s, $i + 1); next; }
        return ( $t, $i + 1 ) if $c eq '}';
        die "PureLua::Cjson: expected ',' or '}' at position $i\n";
    }
}

1;
