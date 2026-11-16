package PureLua::Table;

use strict;
use warnings;

sub new {
    return bless { map => {}, order => [] }, shift;
}

# Wraps a plain Perl string as a Lua string value (PureLua::Eval's
# _mkstr delegates here - this is the one place that owns the
# 'PureLua::Str' class tag). Native Perl code that populates a table
# Lua code will also index - stdlib library tables, the cjson module,
# globals by variable name - must wrap its literal keys with this, or
# they land under a different key than Lua's own string indexing produces.
sub wrap_str {
    my ($s) = @_;
    return bless \$s, 'PureLua::Str';
}

# Canonical key string. Lua strings are blessed PureLua::Str refs and
# Lua numbers are plain Perl scalars (see PureLua::Eval), so a numeric-
# looking string key ("5") and the real number key (5) land in distinct
# slots here, matching real Lua - they used to collapse together under
# an earlier, heuristic-only type distinction; see
# .notes/2026-11-16-pure-lua-engine-progress.md for why that broke.
sub _normalize_key {
    my ($key) = @_;
    die "table index is nil\n" unless defined $key;
    return "s:$$key" if ref($key) eq 'PureLua::Str';
    return "r:$key"  if ref $key;
    my $n = $key + 0;
    $n = int($n) if $n == int($n);
    return "n:$n";
}

sub get {
    my ($self, $key) = @_;
    return undef unless defined $key;
    my $entry = $self->{map}{ _normalize_key($key) };
    return defined($entry) ? $entry->[1] : undef;
}

sub set {
    my ($self, $key, $value) = @_;
    my $ck = _normalize_key($key);
    if (!defined $value) {
        if (exists $self->{map}{$ck}) {
            delete $self->{map}{$ck};
            @{ $self->{order} } = grep { $_ ne $ck } @{ $self->{order} };
        }
        return;
    }
    push @{ $self->{order} }, $ck unless exists $self->{map}{$ck};
    $self->{map}{$ck} = [ $key, $value ];
}

# [key, value] pairs in insertion order (real Lua's pairs() order is
# unspecified; insertion order is a convenient, deterministic choice).
sub pairs_list {
    my ($self) = @_;
    return [ map { $self->{map}{$_} } @{ $self->{order} } ];
}

# Lua's `#t`: a border (n where t[n]~=nil and t[n+1]==nil), found by a
# linear scan from 1. Correct for the sequential-from-1 arrays these
# scripts build; NOT verified against real Lua for sparse or 0-based
# tables - see design doc gotcha #1 (needs an empirical luajit check
# against a `bodyBytes`-shaped table before trusting this further).
sub lua_length {
    my ($self) = @_;
    return 0 unless defined $self->get(1);
    my $n = 1;
    $n++ while defined $self->get($n + 1);
    return $n;
}

1;
