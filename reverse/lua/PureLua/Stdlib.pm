package PureLua::Stdlib;

use strict;
use warnings;

use PureLua::Table;
use PureLua::Pattern;
use PureLua::Cjson;

# Installs exactly the stdlib surface the design doc's grep of the real
# device scripts found (.notes/2026-11-16-pure-perl-lua-engine-design.md):
# math.random/randomseed/floor/modf, os.time, string.char/sub/format/
# gsub/rep/match/find/gmatch, table.insert, pairs, tonumber, tostring,
# bit.band/bor/bxor/lshift/rshift/bnot. Nothing beyond that list -
# `require "cjson"` is deliberately NOT here (see progress notes: the
# project's own cjson.lua shim uses string method-call syntax and
# pattern features - anchors, %c, ?-quantifiers - well outside this
# engine's documented scope, so wiring it up is deferred until actually
# needed, not pre-built speculatively).

sub install {
    my ($class, $eval) = @_;
    my $g = $eval->{globals};

    $g->set(PureLua::Table::wrap_str('tonumber'), sub {
        my ($v, $base) = @_;
        return $eval->_tonumber($v) unless defined $base;
        return _tonumber_base($eval, $eval->str_value($v), int($eval->_tonumber($base)));
    });
    $g->set(PureLua::Table::wrap_str('tostring'), sub { my ($v) = @_; return $eval->lua_str($eval->_tostring($v)); });
    $g->set(PureLua::Table::wrap_str('type'),     sub { my ($v) = @_; return $eval->lua_str($eval->_typename($v)); });
    $g->set(PureLua::Table::wrap_str('print'),    sub { return; }); # debug output only; not consumed by anything downstream.
    $g->set(PureLua::Table::wrap_str('pairs'),    sub { my ($t) = @_; return _pairs($t); });
    $g->set(PureLua::Table::wrap_str('require'),  sub { my ($name) = @_; return _require($eval, $name); });
    $g->set(PureLua::Table::wrap_str('pcall'),    sub {
        my ($fn, @args) = @_;
        my @r = eval { $eval->_call_value($fn, \@args) };
        if (my $err = $@) {
            $err =~ s/\n\z//;
            return ( $eval->lua_bool(0), $eval->lua_str($err) );
        }
        return ( $eval->lua_bool(1), @r );
    });

    $g->set(PureLua::Table::wrap_str('math'),   _build_math($eval));
    $g->set(PureLua::Table::wrap_str('os'),     _build_os());
    $g->set(PureLua::Table::wrap_str('bit'),    _build_bit($eval));
    $g->set(PureLua::Table::wrap_str('string'), _build_string($eval));
    $g->set(PureLua::Table::wrap_str('table'),  _build_table());

    return;
}

my %MODULES = ( cjson => \&_cjson_module, bit => \&_build_bit );

sub _require {
    my ($eval, $name) = @_;
    $name = $eval->str_value($name);
    my $builder = $MODULES{$name};
    die "PureLua::Stdlib: module '$name' is not available (no general module loader - only 'cjson' is built in)\n"
        unless $builder;
    return $builder->($eval);
}

sub _cjson_module {
    my ($eval) = @_;
    my $m = PureLua::Table->new;
    $m->set(PureLua::Table::wrap_str('encode'), sub { my ($v) = @_; return $eval->lua_str( PureLua::Cjson::encode($eval, $v) ); });
    $m->set(PureLua::Table::wrap_str('decode'), sub { my ($s) = @_; return PureLua::Cjson::decode($eval, $eval->str_value($s)); });
    return $m;
}

# Lua's tonumber(s, base): the real device script relies on this to
# parse hex byte pairs (tonumber(doublebytestr, 16)) - a plain-decimal
# parse happened to give the right answer for digit-only pairs ("02")
# but silently returned nil for actual hex letters ("aa"), which emptied
# that table slot entirely (nil assignment deletes a key). Found by
# cross-checking the real device script's dataToJson against luajit.
sub _tonumber_base {
    my ($eval, $s, $base) = @_;
    return undef unless defined $s;
    $s =~ s/\A\s+|\s+\z//g;
    return undef unless length $s;
    my $neg = $s =~ s/\A-//;
    $s =~ s/\A\+//;
    return undef unless length $s;
    my $n = 0;
    for my $c (split //, lc $s) {
        my $d = $c =~ /[0-9]/ ? ord($c) - ord('0')
              : $c =~ /[a-z]/ ? ord($c) - ord('a') + 10
              : return undef;
        return undef if $d >= $base;
        $n = $n * $base + $d;
    }
    return $neg ? -$n : $n;
}

sub _pairs {
    my ($t) = @_;
    die "PureLua::Stdlib: bad argument to 'pairs' (table expected)\n" unless ref($t) eq 'PureLua::Table';
    my $list = $t->pairs_list;
    my $i = 0;
    my $iter = sub {
        return () if $i > $#$list;
        my $pair = $list->[ $i++ ];
        return ( $pair->[0], $pair->[1] );
    };
    return ( $iter, $t, undef );
}

sub _build_math {
    my ($eval) = @_;
    my $math = PureLua::Table->new;
    $math->set(PureLua::Table::wrap_str('floor'), sub { my ($x) = @_; return $eval->_lua_floor($eval->_tonumber($x)); });
    $math->set(PureLua::Table::wrap_str('modf'), sub {
        my ($x) = @_;
        my $n  = $eval->_tonumber($x);
        my $ip = int($n); # Perl's int() truncates toward zero, same as C's modf.
        return ( $ip, $n - $ip );
    });
    $math->set(PureLua::Table::wrap_str('random'), sub {
        my @a = @_;
        return rand() if @a == 0;
        if (@a == 1) {
            my $m = int($eval->_tonumber($a[0]));
            return 1 + int(rand($m));
        }
        my ($m, $n) = ( int($eval->_tonumber($a[0])), int($eval->_tonumber($a[1])) );
        return $m + int(rand($n - $m + 1));
    });
    $math->set(PureLua::Table::wrap_str('fmod'), sub {
        my ($a, $b) = @_;
        $a = $eval->_tonumber($a);
        $b = $eval->_tonumber($b);
        return $a - int($a / $b) * $b; # C fmod semantics: remainder keeps the dividend's sign.
    });
    $math->set(PureLua::Table::wrap_str('randomseed'), sub {
        my ($x) = @_;
        srand(defined $x ? $eval->_tonumber($x) : time());
        return;
    });
    return $math;
}

sub _build_os {
    my $os = PureLua::Table->new;
    $os->set(PureLua::Table::wrap_str('time'), sub { return time(); });
    return $os;
}

# Real LuaJIT's bit library errors loudly on a non-number argument
# ("bad argument #N to 'bor' (number expected, got nil)") rather than
# silently treating it as 0 - matching that strictness turns a script
# bug (or a wrong BASE_CONTROL probe) into the same loud failure real
# Lua would give, instead of a quietly-different "succeeded" result.
# Delegates numeric coercion to $eval->_tonumber rather than its own
# regex+int() so a hex-string value ("0x2010") is parsed via hex(), not
# silently truncated to 0 by Perl's own "+0"/int() (which don't
# understand a "0x" prefix) - the same class of bug _tonumber itself
# had until the wider corpus sweep caught it.
sub _u32 {
    my ($eval, $x) = @_;
    my $n = $eval->_tonumber($x);
    die "bad argument to bit operation (number expected, got " . $eval->_typename($x) . ")\n"
        unless defined $n;
    return int($n) & 0xFFFFFFFF;
}
sub _s32 { my ($x) = @_; return ( $x >= 0x80000000 ) ? $x - 4_294_967_296 : $x; }

sub _build_bit {
    my ($eval) = @_;
    my $bit = PureLua::Table->new;
    $bit->set(PureLua::Table::wrap_str('band'), sub { my $r = 0xFFFFFFFF; $r &= _u32($eval, $_) for @_; return _s32($r); });
    $bit->set(PureLua::Table::wrap_str('bor'),  sub { my $r = 0;          $r |= _u32($eval, $_) for @_; return _s32($r); });
    $bit->set(PureLua::Table::wrap_str('bxor'), sub { my $r = 0;          $r ^= _u32($eval, $_) for @_; return _s32($r); });
    $bit->set(PureLua::Table::wrap_str('bnot'), sub { my ($x) = @_; return _s32( ( ~_u32($eval, $x) ) & 0xFFFFFFFF ); });
    $bit->set(PureLua::Table::wrap_str('lshift'), sub {
        my ($x, $n) = @_;
        return _s32( ( _u32($eval, $x) << ( _u32($eval, $n) & 31 ) ) & 0xFFFFFFFF );
    });
    $bit->set(PureLua::Table::wrap_str('rshift'), sub {
        my ($x, $n) = @_;
        return _s32( ( _u32($eval, $x) >> ( _u32($eval, $n) & 31 ) ) & 0xFFFFFFFF );
    });
    return $bit;
}

sub _str_sub {
    my ($s, $i, $j) = @_;
    my $len = length $s;
    $i = defined $i ? int($i) : 1;
    $j = defined $j ? int($j) : -1;
    $i = $len + $i + 1 if $i < 0;
    $j = $len + $j + 1 if $j < 0;
    $i = 1 if $i < 1;
    $j = $len if $j > $len;
    return '' if $i > $j;
    return substr($s, $i - 1, $j - $i + 1);
}

# Lua's %q: wraps a string in double quotes, escaping the characters
# that would otherwise break re-reading it back as a Lua literal.
sub _format_q {
    my ($v) = @_;
    return 'nil' unless defined $v;
    my $s = ref($v) eq 'PureLua::Str' ? $$v : $v;
    $s =~ s/([\\"])/\\$1/g;
    $s =~ s/\n/\\\n/g;
    $s =~ s/\r/\\r/g;
    $s =~ s/\0/\\0/g;
    return '"' . $s . '"';
}

# Narrow: handles whatever conversion letter is asked for by handing a
# single directive straight to Perl's own sprintf (close enough to
# Lua's for %d/%s/%x/%02x etc.), except %q which Lua defines specially.
sub _format {
    my ($fmt, $args) = @_;
    $fmt = $$fmt if ref($fmt) eq 'PureLua::Str';
    my $out = '';
    my $ai  = 0;
    my $i   = 0;
    my $len = length $fmt;

    while ($i < $len) {
        my $c = substr($fmt, $i, 1);
        if ($c ne '%') { $out .= $c; $i++; next; }
        if (substr($fmt, $i + 1, 1) eq '%') { $out .= '%'; $i += 2; next; }

        my $rest = substr($fmt, $i);
        $rest =~ /\A%([-+0 #]*)(\d*)(\.\d+)?([a-zA-Z])/
            or die "PureLua::Stdlib: malformed format directive in '$fmt' at position $i\n";
        my $directive = $&;
        my $conv       = $4;
        $i += length($directive);
        my $arg = $args->[ $ai++ ];

        if ($conv eq 'q') {
            $out .= _format_q($arg);
        }
        else {
            $arg = $$arg if ref($arg) eq 'PureLua::Str';
            $out .= sprintf($directive, $arg);
        }
    }
    return $out;
}

sub _build_string {
    my ($eval) = @_;
    my $str = PureLua::Table->new;

    my $sv = sub { return $eval->str_value( $_[0] ); }; # unwrap a Lua string arg to plain Perl text

    $str->set(PureLua::Table::wrap_str('char'),    sub { return $eval->lua_str( join( '', map { chr(int($_)) } @_ ) ); });
    $str->set(PureLua::Table::wrap_str('sub'),     sub { my ($s, $i, $j) = @_; return $eval->lua_str( _str_sub($sv->($s), $i, $j) ); });
    $str->set(PureLua::Table::wrap_str('rep'), sub {
        my ($s, $n) = @_;
        $s = $sv->($s);
        $n = int($n);
        return $eval->lua_str( $n > 0 ? ( $s x $n ) : '' );
    });
    $str->set(PureLua::Table::wrap_str('reverse'), sub { my ($s) = @_; return $eval->lua_str( scalar reverse $sv->($s) ); });
    $str->set(PureLua::Table::wrap_str('format'),  sub { my ($fmt, @args) = @_; return $eval->lua_str( _format($fmt, \@args) ); });
    $str->set(PureLua::Table::wrap_str('byte'), sub {
        my ($s, $i, $j) = @_;
        $s = $sv->($s);
        $i = defined $i ? int($i) : 1;
        $j = defined $j ? int($j) : $i;
        my $len = length $s;
        $i = $len + $i + 1 if $i < 0;
        $j = $len + $j + 1 if $j < 0;
        $i = 1 if $i < 1;
        $j = $len if $j > $len;
        return () if $i > $j;
        return map { ord( substr($s, $_ - 1, 1) ) } $i .. $j;
    });

    $str->set(PureLua::Table::wrap_str('find'), sub {
        my ($s, $p, $init) = @_;
        return PureLua::Pattern::find($sv->($s), $sv->($p), $init);
    });
    $str->set(PureLua::Table::wrap_str('match'), sub {
        my ($s, $p, $init) = @_;
        my $m = PureLua::Pattern::match($sv->($s), $sv->($p), $init);
        return defined $m ? $eval->lua_str($m) : ();
    });
    $str->set(PureLua::Table::wrap_str('gmatch'), sub {
        my ($s, $p) = @_;
        my $it = PureLua::Pattern::gmatch_iterator($sv->($s), $sv->($p));
        return sub { my $m = $it->(); return defined $m ? $eval->lua_str($m) : (); };
    });
    $str->set(PureLua::Table::wrap_str('gsub'), sub {
        my ($s, $p, $repl, $n) = @_;
        my $perl_repl = ( ref($repl) eq 'PureLua::Function' || ref($repl) eq 'CODE' )
            ? sub {
                my ($matched) = @_;
                my @r  = $eval->_call_value($repl, [ $eval->lua_str($matched) ]);
                my $rv = $r[0];
                return PureLua::Eval::_is_truthy($rv) ? $eval->_tostring($rv) : undef;
            }
            : $sv->($repl);
        my ($out, $count) = PureLua::Pattern::gsub($sv->($s), $sv->($p), $perl_repl, $n);
        return ( $eval->lua_str($out), $count );
    });
    return $str;
}

sub _build_table {
    my $table = PureLua::Table->new;
    $table->set(PureLua::Table::wrap_str('insert'), sub {
        my ($t, @rest) = @_;
        if (@rest == 1) {
            $t->set($t->lua_length + 1, $rest[0]);
        }
        else {
            my ($pos, $val) = @rest;
            for (my $i = $t->lua_length; $i >= $pos; $i--) {
                $t->set($i + 1, $t->get($i));
            }
            $t->set($pos, $val);
        }
        return;
    });
    return $table;
}

1;
