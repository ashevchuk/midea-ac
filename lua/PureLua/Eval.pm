package PureLua::Eval;

use strict;
use warnings;

use PureLua::Table;

# Lua's truthiness excludes only nil and false - 0 and "" are truthy, so
# booleans need a representation distinct from Perl's own notion of
# false. Blessed singleton scalars, checked by ref(), carry that.
my $TRUE  = bless( \( my $t = 1 ), 'PureLua::Boolean' );
my $FALSE = bless( \( my $f = 0 ), 'PureLua::Boolean' );

# Lua strings are blessed scalar refs; Lua numbers stay plain,
# unblessed Perl scalars. This split exists because Perl itself can't
# otherwise tell "the string '00'" apart from "the number 0" - without
# it, a numeric-looking string fed back through _tostring() (e.g. during
# `..` concatenation) gets silently re-formatted as a number, which
# corrupted real protocol hex strings (string.format("%02x", 0) -> "00",
# then "s..that" collapsed it back down to "0", dropping the leading
# zero - found while cross-checking the real device script against
# luajit; see .notes/2026-11-16-pure-lua-engine-progress.md).
sub _mkstr { my ($s) = @_; return PureLua::Table::wrap_str($s); }

sub _is_truthy {
    my ($v) = @_;
    return 0 unless defined $v;
    return 0 if ref($v) eq 'PureLua::Boolean' && !$$v;
    return 1;
}

sub _bool { return $_[0] ? $TRUE : $FALSE; }

# Control-flow signals, thrown via die and caught at the matching
# boundary (function call for Return, loop body for Break) - the
# standard way to unwind a tree-walking interpreter through arbitrarily
# nested blocks without threading a signal value through every call site.
package PureLua::Signal::Return;
sub new { my ($class, $values) = @_; return bless { values => $values }, $class; }

package PureLua::Signal::Break;
sub new { return bless {}, shift; }

package PureLua::Eval;

sub new {
    my ($class) = @_;
    return bless { globals => PureLua::Table->new }, $class;
}

# Public: lets sibling modules (e.g. PureLua::Cjson, decoding JSON
# true/false) build a real Lua boolean without reaching into this
# module's private $TRUE/$FALSE singletons directly.
sub lua_bool { my ($self, $truthy) = @_; return _bool($truthy); }

# Public: wrap a plain Perl string as a real Lua string value, and
# unwrap one back to plain Perl text (passes non-Str scalars through
# unchanged, so callers can use it defensively on an already-plain
# value without checking first).
sub lua_str   { my ($self, $s) = @_; return _mkstr($s); }
sub str_value { my ($self, $v) = @_; return ref($v) eq 'PureLua::Str' ? $$v : $v; }

# --- scope / environment -------------------------------------------------
#
# Each scope is {vars=>{name=>\$scalar}, parent=>$scope_or_undef}. A
# fresh scalar ref per `local` declaration is what makes closures over
# loop variables work: each loop iteration gets its own cell, so
# closures created in different iterations don't alias each other
# (real Lua 5.1 semantics - see t/eval.t's "fresh loop variable" case).
# Globals are not scope entries at all - they live in one shared Table,
# exactly as real Lua desugars a global reference to `_G.name`.

sub _new_scope {
    my ($self, $parent) = @_;
    return { vars => {}, parent => $parent };
}

sub _declare_local {
    my ($self, $scope, $name, $value) = @_;
    $scope->{vars}{$name} = \$value;
}

sub _lookup_cell {
    my ($self, $scope, $name) = @_;
    while ($scope) {
        return $scope->{vars}{$name} if exists $scope->{vars}{$name};
        $scope = $scope->{parent};
    }
    return undef;
}

sub _get_var {
    my ($self, $scope, $name) = @_;
    my $cell = $self->_lookup_cell($scope, $name);
    return $cell ? $$cell : $self->{globals}->get(PureLua::Table::wrap_str($name));
}

sub _set_var {
    my ($self, $scope, $name, $value) = @_;
    my $cell = $self->_lookup_cell($scope, $name);
    if ($cell) { $$cell = $value; }
    else       { $self->{globals}->set(PureLua::Table::wrap_str($name), $value); }
}

sub _get_varargs {
    my ($self, $scope) = @_;
    while ($scope) {
        return $scope->{varargs} if exists $scope->{varargs};
        $scope = $scope->{parent};
    }
    return [];
}

sub _runtime_error {
    my ($self, $msg, $line) = @_;
    die "PureLua::Eval: $msg" . (defined $line ? " at line $line" : '') . "\n";
}

# --- public entry point ---------------------------------------------------

# Runs a parsed chunk's top-level block, returning whatever its
# top-level `return` yields (empty list if it falls off the end).
sub run {
    my ($self, $chunk, $args) = @_;
    my $scope = $self->_new_scope(undef);
    $scope->{varargs} = $args || [];
    eval { $self->_exec_block($scope, $chunk->{body}); };
    my $err = $@;
    if ($err) {
        return @{ $err->{values} } if ref($err) eq 'PureLua::Signal::Return';
        die $err;
    }
    return ();
}

# --- statements ------------------------------------------------------------

sub _exec_block {
    my ($self, $scope, $stmts) = @_;
    $self->_exec_stmt($scope, $_) for @$stmts;
}

sub _exec_stmt {
    my ($self, $scope, $stmt) = @_;
    my $type = $stmt->{type};

    if ($type eq 'local') {
        my @vals = $self->_eval_explist($scope, $stmt->{exprs});
        my $names = $stmt->{names};
        $self->_declare_local($scope, $names->[$_], $vals[$_]) for 0 .. $#$names;
        return;
    }
    if ($type eq 'localfunction') {
        # Desugars to `local f; f = function() ... end`, not `local f =
        # function()...end` - the cell must exist before the closure is
        # built so the function body can call itself recursively.
        $self->_declare_local($scope, $stmt->{name}, undef);
        my $cell = $self->_lookup_cell($scope, $stmt->{name});
        $$cell = $self->_make_function($scope, $stmt->{body});
        return;
    }
    if ($type eq 'assign') {
        $self->_exec_assign($scope, $stmt->{targets}, $stmt->{exprs});
        return;
    }
    if ($type eq 'callstat') {
        $self->_eval_call_multi($scope, $stmt->{call});
        return;
    }
    if ($type eq 'do') {
        $self->_exec_block($self->_new_scope($scope), $stmt->{body});
        return;
    }
    if ($type eq 'if') {
        for my $clause (@{ $stmt->{clauses} }) {
            if (_is_truthy($self->_eval_expr($scope, $clause->{cond}))) {
                $self->_exec_block($self->_new_scope($scope), $clause->{body});
                return;
            }
        }
        $self->_exec_block($self->_new_scope($scope), $stmt->{else_body}) if $stmt->{else_body};
        return;
    }
    if ($type eq 'while') {
        while (_is_truthy($self->_eval_expr($scope, $stmt->{cond}))) {
            last unless $self->_run_loop_body($self->_new_scope($scope), $stmt->{body});
        }
        return;
    }
    if ($type eq 'repeat') {
        while (1) {
            my $iter_scope = $self->_new_scope($scope);
            my $broke = 0;
            eval { $self->_exec_block($iter_scope, $stmt->{body}); };
            if (my $err = $@) {
                die $err unless ref($err) eq 'PureLua::Signal::Break';
                $broke = 1;
            }
            last if $broke;
            last if _is_truthy($self->_eval_expr($iter_scope, $stmt->{cond}));
        }
        return;
    }
    if ($type eq 'fornum') {
        $self->_exec_fornum($scope, $stmt);
        return;
    }
    if ($type eq 'forin') {
        $self->_exec_forin($scope, $stmt);
        return;
    }
    if ($type eq 'function') {
        $self->_assign_to($scope, $stmt->{target}, $self->_make_function($scope, $stmt->{body}));
        return;
    }
    if ($type eq 'return') {
        die PureLua::Signal::Return->new([ $self->_eval_explist($scope, $stmt->{exprs}) ]);
    }
    if ($type eq 'break') {
        die PureLua::Signal::Break->new;
    }
    $self->_runtime_error("unknown statement type '$type'", $stmt->{line});
}

# Runs a loop iteration's block in the given (already-fresh) scope;
# returns 1 to keep looping, 0 if the body broke out.
sub _run_loop_body {
    my ($self, $scope, $body) = @_;
    eval { $self->_exec_block($scope, $body); };
    if (my $err = $@) {
        die $err unless ref($err) eq 'PureLua::Signal::Break';
        return 0;
    }
    return 1;
}

sub _exec_fornum {
    my ($self, $scope, $stmt) = @_;
    my $line  = $stmt->{line};
    my $start = $self->_tonumber_strict($self->_eval_expr($scope, $stmt->{start}), $line);
    my $stop  = $self->_tonumber_strict($self->_eval_expr($scope, $stmt->{stop}), $line);
    my $step  = defined $stmt->{step}
        ? $self->_tonumber_strict($self->_eval_expr($scope, $stmt->{step}), $line)
        : 1;
    $self->_runtime_error("'for' step is zero", $line) if $step == 0;

    my $val = $start;
    while ($step > 0 ? $val <= $stop : $val >= $stop) {
        my $iter_scope = $self->_new_scope($scope);
        $self->_declare_local($iter_scope, $stmt->{var}, $val);
        last unless $self->_run_loop_body($iter_scope, $stmt->{body});
        $val += $step;
    }
}

sub _exec_forin {
    my ($self, $scope, $stmt) = @_;
    my ($iter_func, $state, $ctrl) = $self->_eval_explist($scope, $stmt->{exprs});

    while (1) {
        my @results = $self->_call_value($iter_func, [ $state, $ctrl ], $stmt->{line});
        last unless defined $results[0];
        $ctrl = $results[0];

        my $iter_scope = $self->_new_scope($scope);
        my $names = $stmt->{names};
        $self->_declare_local($iter_scope, $names->[$_], $results[$_]) for 0 .. $#$names;
        last unless $self->_run_loop_body($iter_scope, $stmt->{body});
    }
}

sub _exec_assign {
    my ($self, $scope, $targets, $exprs) = @_;
    # Evaluate every target's object/key before evaluating the RHS - real
    # Lua's exact cross-side ordering is unspecified, but reading index
    # expressions before any write happens is the least surprising choice.
    my @prepared;
    for my $t (@$targets) {
        if ($t->{type} eq 'name') {
            push @prepared, { name => $t->{name} };
        }
        else {
            push @prepared, {
                obj => $self->_eval_expr($scope, $t->{obj}),
                key => $self->_eval_expr($scope, $t->{key}),
            };
        }
    }
    my @vals = $self->_eval_explist($scope, $exprs);
    for my $i (0 .. $#prepared) {
        my $p = $prepared[$i];
        if (exists $p->{name}) { $self->_set_var($scope, $p->{name}, $vals[$i]); }
        else                   { $self->_table_set($p->{obj}, $p->{key}, $vals[$i]); }
    }
}

sub _assign_to {
    my ($self, $scope, $target, $value) = @_;
    if ($target->{type} eq 'name') {
        $self->_set_var($scope, $target->{name}, $value);
    }
    else {
        my $obj = $self->_eval_expr($scope, $target->{obj});
        my $key = $self->_eval_expr($scope, $target->{key});
        $self->_table_set($obj, $key, $value);
    }
}

# --- expressions ------------------------------------------------------------
#
# `_eval_expr` always yields exactly one value (Lua's "adjust to 1"
# rule). `_eval_multi`/`_eval_explist` implement the one place that
# rule doesn't apply: the *last* expression in a list expands to all of
# its results if it's a call or `...`.

sub _eval_explist {
    my ($self, $scope, $exprs) = @_;
    return () unless @$exprs;
    my @out = map { $self->_eval_expr($scope, $exprs->[$_]) } 0 .. $#$exprs - 1;
    push @out, $self->_eval_multi($scope, $exprs->[-1]);
    return @out;
}

sub _eval_multi {
    my ($self, $scope, $node) = @_;
    my $type = $node->{type};
    return $self->_eval_call_multi($scope, $node)       if $type eq 'call';
    return $self->_eval_methodcall_multi($scope, $node) if $type eq 'methodcall';
    return @{ $self->_get_varargs($scope) }              if $type eq 'vararg';
    return ( $self->_eval_expr($scope, $node) );
}

sub _eval_call_multi {
    my ($self, $scope, $node) = @_;
    my $func = $self->_eval_expr($scope, $node->{func});
    my @args = $self->_eval_explist($scope, $node->{args});
    return $self->_call_value($func, \@args, $node->{line});
}

sub _eval_methodcall_multi {
    my ($self, $scope, $node) = @_;
    my $obj = $self->_eval_expr($scope, $node->{obj});

    # Real Lua gives strings a built-in metatable routing `s:method(...)`
    # through the `string` library - the one hard-wired metatable this
    # engine supports, not a general metatable mechanism.
    my $func;
    if (ref($obj) eq 'PureLua::Str') {
        my $strlib = $self->{globals}->get(PureLua::Table::wrap_str('string'));
        $func = $strlib->get(PureLua::Table::wrap_str($node->{method})) if $strlib;
        $self->_runtime_error("attempt to call method '$node->{method}' (no string library installed)", $node->{line})
            unless defined $func;
    }
    else {
        $func = $self->_table_get($obj, PureLua::Table::wrap_str($node->{method}), $node->{line});
    }
    my @args = $self->_eval_explist($scope, $node->{args});
    return $self->_call_value($func, [ $obj, @args ], $node->{line});
}

sub _call_value {
    my ($self, $func, $args, $line) = @_;
    return $func->(@$args) if ref($func) eq 'CODE';
    return $self->_call_lua_function($func, $args) if ref($func) eq 'PureLua::Function';
    $self->_runtime_error('attempt to call a non-function value', $line);
}

sub _call_lua_function {
    my ($self, $func, $args) = @_;
    my $scope  = $self->_new_scope($func->{closure_env});
    my $params = $func->{params};
    $self->_declare_local($scope, $params->[$_], $args->[$_]) for 0 .. $#$params;
    if ($func->{vararg}) {
        my @extra = @$args[ scalar(@$params) .. $#$args ];
        $scope->{varargs} = \@extra;
    }
    eval { $self->_exec_block($scope, $func->{body}); };
    my $err = $@;
    if ($err) {
        return @{ $err->{values} } if ref($err) eq 'PureLua::Signal::Return';
        die $err;
    }
    return ();
}

sub _make_function {
    my ($self, $scope, $funcbody) = @_;
    return bless {
        params      => $funcbody->{params},
        vararg      => $funcbody->{vararg},
        body        => $funcbody->{body},
        closure_env => $scope,
    }, 'PureLua::Function';
}

sub _eval_expr {
    my ($self, $scope, $node) = @_;
    my $type = $node->{type};

    return undef                                   if $type eq 'nil';
    return $TRUE                                    if $type eq 'true';
    return $FALSE                                    if $type eq 'false';
    return $node->{value}                            if $type eq 'number';
    return _mkstr($node->{value})                    if $type eq 'string';
    return $self->_get_var($scope, $node->{name})    if $type eq 'name';
    return $self->_eval_expr($scope, $node->{expr})  if $type eq 'paren';
    return $self->_get_varargs($scope)->[0]          if $type eq 'vararg';
    return $self->_make_function($scope, $node->{body}) if $type eq 'function';
    return $self->_eval_table($scope, $node)         if $type eq 'table';
    return $self->_eval_binop($scope, $node)         if $type eq 'binop';
    return $self->_eval_unop($scope, $node)          if $type eq 'unop';

    if ($type eq 'index') {
        my $obj = $self->_eval_expr($scope, $node->{obj});
        my $key = $self->_eval_expr($scope, $node->{key});
        return $self->_table_get($obj, $key, $node->{line});
    }
    if ($type eq 'call') {
        my @r = $self->_eval_call_multi($scope, $node);
        return $r[0];
    }
    if ($type eq 'methodcall') {
        my @r = $self->_eval_methodcall_multi($scope, $node);
        return $r[0];
    }
    $self->_runtime_error("unknown expression type '$type'", $node->{line});
}

sub _eval_table {
    my ($self, $scope, $node) = @_;
    my $tbl    = PureLua::Table->new;
    my $fields = $node->{fields};
    my $next_index = 1;

    for my $i (0 .. $#$fields) {
        my $f = $fields->[$i];
        if ($f->{type} eq 'named') {
            $tbl->set(_mkstr($f->{key}), $self->_eval_expr($scope, $f->{value}));
        }
        elsif ($f->{type} eq 'keyed') {
            $tbl->set($self->_eval_expr($scope, $f->{key}), $self->_eval_expr($scope, $f->{value}));
        }
        elsif ($i == $#$fields) {
            # Only the last positional field expands to multiple slots.
            $tbl->set($next_index++, $_) for $self->_eval_multi($scope, $f->{value});
        }
        else {
            $tbl->set($next_index++, $self->_eval_expr($scope, $f->{value}));
        }
    }
    return $tbl;
}

sub _table_get {
    my ($self, $obj, $key, $line) = @_;
    return $obj->get($key) if ref($obj) eq 'PureLua::Table';
    $self->_runtime_error('attempt to index a ' . $self->_typename($obj) . ' value', $line);
}

sub _table_set {
    my ($self, $obj, $key, $value, $line) = @_;
    if (ref($obj) eq 'PureLua::Table') { $obj->set($key, $value); return; }
    $self->_runtime_error('attempt to index a ' . $self->_typename($obj) . ' value', $line);
}

sub _typename {
    my ($self, $v) = @_;
    return 'nil' unless defined $v;
    return 'boolean'  if ref($v) eq 'PureLua::Boolean';
    return 'table'    if ref($v) eq 'PureLua::Table';
    return 'function' if ref($v) eq 'PureLua::Function' || ref($v) eq 'CODE';
    return 'string'   if ref($v) eq 'PureLua::Str';
    return 'number'; # unblessed and unreffed - with strings wrapped, nothing else is left
}

# Arithmetic coercion: a real Lua number passes straight through; a Lua
# string coerces only if its text looks numeric (Lua's own rule, e.g.
# "5"+2 == 7); anything else (nil/boolean/table/function) isn't coercible.
sub _tonumber {
    my ($self, $v) = @_;
    return undef unless defined $v;
    return $v + 0 unless ref $v;
    return undef unless ref($v) eq 'PureLua::Str';
    my $s = $$v;
    # Perl's own string-to-number coercion (`+0`) does NOT understand a
    # "0x..." prefix (`"0x10" + 0` is 0, not 16) - found via the wider
    # corpus sweep tripping over a literal `"0x2010"` string value real
    # Lua auto-coerces correctly. The hex branch needs `hex()` explicitly.
    if ($s =~ /\A\s*(-?)0[xX]([0-9a-fA-F]+)\s*\z/) {
        my $n = hex($2);
        return $1 eq '-' ? -$n : $n;
    }
    return $s + 0 if $s =~ /\A\s*-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?\s*\z/;
    return undef;
}

sub _tonumber_strict {
    my ($self, $v, $line) = @_;
    my $n = $self->_tonumber($v);
    $self->_runtime_error('attempt to perform arithmetic on a ' . $self->_typename($v) . ' value', $line)
        unless defined $n;
    return $n;
}

# Returns a PLAIN Perl string - the text itself, not a Lua value. Used
# both internally (error messages) and as the building block for
# anything that DOES need to hand a real Lua string back (string.format,
# `..`, tostring()), which wrap this return value themselves. Critically,
# an actual Lua string passes through verbatim, never reformatted - that
# was the bug: re-running an already-correct "00" through a number
# formatter silently dropped its leading zero.
sub _tostring {
    my ($self, $v) = @_;
    return 'nil' unless defined $v;
    return $$v ? 'true' : 'false' if ref($v) eq 'PureLua::Boolean';
    return "table: $v"            if ref($v) eq 'PureLua::Table';
    return "function: $v"         if ref($v) eq 'PureLua::Function' || ref($v) eq 'CODE';
    return $$v                    if ref($v) eq 'PureLua::Str';
    return sprintf('%.14g', $v); # must be a number
}

sub _lua_floor {
    my ($self, $x) = @_;
    my $i = int($x);
    return ($i > $x) ? $i - 1 : $i;
}

sub _values_equal {
    my ($self, $l, $r) = @_;
    my ($tl, $tr) = ($self->_typename($l), $self->_typename($r));
    return 0 if $tl ne $tr;
    return 1 if $tl eq 'nil';
    return $l == $r if $tl eq 'number';
    return $$l eq $$r if $tl eq 'string';
    return (!!$$l) == (!!$$r) if $tl eq 'boolean';
    return $l == $r; # table/function: reference identity
}

sub _compare {
    my ($self, $op, $l, $r, $line) = @_;
    my ($tl, $tr) = ($self->_typename($l), $self->_typename($r));
    $self->_runtime_error("attempt to compare $tl with $tr", $line) if $tl ne $tr;
    $self->_runtime_error("attempt to compare two $tl values", $line)
        unless $tl eq 'number' || $tl eq 'string';

    my ($lv, $rv) = $tl eq 'string' ? ( $$l, $$r ) : ( $l, $r );
    my $result =
        $op eq '<'  ? ($tl eq 'number' ? $lv < $rv  : $lv lt $rv) :
        $op eq '<=' ? ($tl eq 'number' ? $lv <= $rv : $lv le $rv) :
        $op eq '>'  ? ($tl eq 'number' ? $lv > $rv  : $lv gt $rv) :
                      ($tl eq 'number' ? $lv >= $rv : $lv ge $rv);
    return _bool($result);
}

sub _eval_binop {
    my ($self, $scope, $node) = @_;
    my $op = $node->{op};

    if ($op eq 'and') {
        my $l = $self->_eval_expr($scope, $node->{lhs});
        return $l unless _is_truthy($l);
        return $self->_eval_expr($scope, $node->{rhs});
    }
    if ($op eq 'or') {
        my $l = $self->_eval_expr($scope, $node->{lhs});
        return $l if _is_truthy($l);
        return $self->_eval_expr($scope, $node->{rhs});
    }

    my $l    = $self->_eval_expr($scope, $node->{lhs});
    my $r    = $self->_eval_expr($scope, $node->{rhs});
    my $line = $node->{line};

    return $self->_tonumber_strict($l, $line) + $self->_tonumber_strict($r, $line) if $op eq '+';
    return $self->_tonumber_strict($l, $line) - $self->_tonumber_strict($r, $line) if $op eq '-';
    return $self->_tonumber_strict($l, $line) * $self->_tonumber_strict($r, $line) if $op eq '*';
    return $self->_tonumber_strict($l, $line) / $self->_tonumber_strict($r, $line) if $op eq '/';
    return $self->_tonumber_strict($l, $line) ** $self->_tonumber_strict($r, $line) if $op eq '^';
    if ($op eq '%') {
        my ($a, $b) = ($self->_tonumber_strict($l, $line), $self->_tonumber_strict($r, $line));
        return $a - $self->_lua_floor($a / $b) * $b;
    }
    return _mkstr($self->_tostring($l) . $self->_tostring($r)) if $op eq '..';

    return _bool($self->_values_equal($l, $r))  if $op eq '==';
    return _bool(!$self->_values_equal($l, $r)) if $op eq '~=';
    return $self->_compare($op, $l, $r, $line)  if $op eq '<' || $op eq '<=' || $op eq '>' || $op eq '>=';

    $self->_runtime_error("unknown binary operator '$op'", $line);
}

sub _eval_unop {
    my ($self, $scope, $node) = @_;
    my $v  = $self->_eval_expr($scope, $node->{expr});
    my $op = $node->{op};

    return -$self->_tonumber_strict($v, $node->{line}) if $op eq '-';
    return _is_truthy($v) ? $FALSE : $TRUE              if $op eq 'not';
    if ($op eq '#') {
        return $v->lua_length if ref($v) eq 'PureLua::Table';
        return length($$v) if ref($v) eq 'PureLua::Str';
        $self->_runtime_error('attempt to get length of a ' . $self->_typename($v) . ' value', $node->{line});
    }
    $self->_runtime_error("unknown unary operator '$op'", $node->{line});
}

1;
