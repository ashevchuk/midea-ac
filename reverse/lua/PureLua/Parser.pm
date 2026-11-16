package PureLua::Parser;

use strict;
use warnings;

use PureLua::Lexer;

# Recursive-descent parser for the Lua 5.1 grammar (reference manual §8),
# producing a plain-hashref AST. No goto/labels (5.2+ only, not in the 5.1
# grammar at all). Metatables/coroutines are library identifiers, not
# syntax, so they need nothing special here.

# Binary operator precedence: { op => [left_binding, right_binding] }.
# Higher number binds tighter. right_binding < left_binding makes an
# operator right-associative (lets the same operator recurse on the
# right); right_binding == left_binding makes it left-associative (the
# caller's own while-loop picks up the next same-level operator instead).
my %BINOP = (
    'or' => [1, 1],
    'and' => [2, 2],
    '<' => [3, 3], '>' => [3, 3], '<=' => [3, 3], '>=' => [3, 3],
    '==' => [3, 3], '~=' => [3, 3],
    '..' => [4, 3],
    '+' => [5, 5], '-' => [5, 5],
    '*' => [6, 6], '/' => [6, 6], '%' => [6, 6],
    '^' => [8, 7],
);

# Binding power used when parsing a unary operator's operand: high enough
# to exclude everything except '^', which binds tighter than unary minus
# (so `-x^2` parses as `-(x^2)`).
my $UNARY_PREC = 7;

my %BLOCK_END_KEYWORDS = map { $_ => 1 } qw(end else elseif until);

sub parse {
    my ($class, $src_or_tokens) = @_;
    my $tokens = ref($src_or_tokens) eq 'ARRAY'
        ? $src_or_tokens
        : PureLua::Lexer->tokenize($src_or_tokens);
    my $self = bless { tokens => $tokens, idx => 0 }, $class;

    my $body = $self->_parse_block;
    $self->_expect('eof');
    return { type => 'chunk', body => $body };
}

# --- token cursor helpers ---------------------------------------------

sub _cur  { my ($self, $off) = @_; return $self->{tokens}[$self->{idx} + ($off || 0)]; }
sub _check    { my ($self, $type) = @_; return $self->_cur->{type} eq $type; }
sub _check_kw { my ($self, $kw)   = @_; my $t = $self->_cur; return $t->{type} eq 'keyword' && $t->{value} eq $kw; }

sub _advance {
    my ($self) = @_;
    my $t = $self->_cur;
    $self->{idx}++;
    return $t;
}

sub _error {
    my ($self, $msg) = @_;
    my $t = $self->_cur;
    my $got = $t->{type} eq 'eof' ? 'eof' : ($t->{value} // $t->{type});
    die "PureLua::Parser: $msg (got '$got') at line $t->{line}\n";
}

sub _expect {
    my ($self, $type) = @_;
    $self->_error("expected '$type'") unless $self->_check($type);
    return $self->_advance;
}

sub _expect_kw {
    my ($self, $kw) = @_;
    $self->_error("expected '$kw'") unless $self->_check_kw($kw);
    return $self->_advance;
}

sub _expect_name {
    my ($self) = @_;
    $self->_error('expected identifier') unless $self->_check('name');
    return $self->_advance->{value};
}

sub _at_block_end {
    my ($self) = @_;
    my $t = $self->_cur;
    return 1 if $t->{type} eq 'eof';
    return 1 if $t->{type} eq 'keyword' && $BLOCK_END_KEYWORDS{ $t->{value} };
    return 0;
}

sub _at_laststat {
    my ($self) = @_;
    my $t = $self->_cur;
    return $t->{type} eq 'keyword' && ($t->{value} eq 'return' || $t->{value} eq 'break');
}

# --- statements ---------------------------------------------------------

sub _parse_block {
    my ($self) = @_;
    my @stmts;
    while (!$self->_at_block_end) {
        if ($self->_check(';')) {
            $self->_advance;
            next;
        }
        if ($self->_at_laststat) {
            push @stmts, $self->_parse_laststat;
            $self->_advance if $self->_check(';');
            last;
        }
        push @stmts, $self->_parse_statement;
        $self->_advance if $self->_check(';');
    }
    return \@stmts;
}

sub _parse_laststat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    if ($self->_check_kw('break')) {
        $self->_advance;
        return { type => 'break', line => $line };
    }
    $self->_expect_kw('return');
    my $exprs = [];
    unless ($self->_check(';') || $self->_at_block_end) {
        $exprs = $self->_parse_explist;
    }
    return { type => 'return', exprs => $exprs, line => $line };
}

sub _parse_statement {
    my ($self) = @_;
    my $line = $self->_cur->{line};

    return $self->_parse_if       if $self->_check_kw('if');
    return $self->_parse_while    if $self->_check_kw('while');
    return $self->_parse_repeat   if $self->_check_kw('repeat');
    return $self->_parse_for      if $self->_check_kw('for');
    return $self->_parse_function_stat if $self->_check_kw('function');
    return $self->_parse_local    if $self->_check_kw('local');

    if ($self->_check_kw('do')) {
        $self->_advance;
        my $body = $self->_parse_block;
        $self->_expect_kw('end');
        return { type => 'do', body => $body, line => $line };
    }

    return $self->_parse_exprstat;
}

sub _parse_if {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('if');
    my @clauses;
    my $cond = $self->_parse_expr(0);
    $self->_expect_kw('then');
    my $body = $self->_parse_block;
    push @clauses, { cond => $cond, body => $body };

    while ($self->_check_kw('elseif')) {
        $self->_advance;
        my $c = $self->_parse_expr(0);
        $self->_expect_kw('then');
        my $b = $self->_parse_block;
        push @clauses, { cond => $c, body => $b };
    }

    my $else_body;
    if ($self->_check_kw('else')) {
        $self->_advance;
        $else_body = $self->_parse_block;
    }
    $self->_expect_kw('end');
    return { type => 'if', clauses => \@clauses, else_body => $else_body, line => $line };
}

sub _parse_while {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('while');
    my $cond = $self->_parse_expr(0);
    $self->_expect_kw('do');
    my $body = $self->_parse_block;
    $self->_expect_kw('end');
    return { type => 'while', cond => $cond, body => $body, line => $line };
}

sub _parse_repeat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('repeat');
    my $body = $self->_parse_block;
    $self->_expect_kw('until');
    my $cond = $self->_parse_expr(0);
    return { type => 'repeat', body => $body, cond => $cond, line => $line };
}

sub _parse_for {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('for');
    my $name1 = $self->_expect_name;

    if ($self->_check('=')) {
        $self->_advance;
        my $start = $self->_parse_expr(0);
        $self->_expect(',');
        my $stop = $self->_parse_expr(0);
        my $step;
        if ($self->_check(',')) {
            $self->_advance;
            $step = $self->_parse_expr(0);
        }
        $self->_expect_kw('do');
        my $body = $self->_parse_block;
        $self->_expect_kw('end');
        return { type => 'fornum', var => $name1, start => $start, stop => $stop, step => $step, body => $body, line => $line };
    }

    my @names = ($name1);
    while ($self->_check(',')) {
        $self->_advance;
        push @names, $self->_expect_name;
    }
    $self->_expect_kw('in');
    my $exprs = $self->_parse_explist;
    $self->_expect_kw('do');
    my $body = $self->_parse_block;
    $self->_expect_kw('end');
    return { type => 'forin', names => \@names, exprs => $exprs, body => $body, line => $line };
}

sub _parse_function_stat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('function');

    my $target = { type => 'name', name => $self->_expect_name };
    while ($self->_check('.')) {
        $self->_advance;
        my $field = $self->_expect_name;
        $target = { type => 'index', obj => $target, key => { type => 'string', value => $field } };
    }

    my $is_method = 0;
    if ($self->_check(':')) {
        $self->_advance;
        my $field = $self->_expect_name;
        $target = { type => 'index', obj => $target, key => { type => 'string', value => $field } };
        $is_method = 1;
    }

    my $body = $self->_parse_funcbody($is_method);
    return { type => 'function', target => $target, body => $body, line => $line };
}

sub _parse_funcbody {
    my ($self, $is_method) = @_;
    $self->_expect('(');
    my @params = $is_method ? ('self') : ();
    my $vararg = 0;

    unless ($self->_check(')')) {
        while (1) {
            if ($self->_check('...')) {
                $self->_advance;
                $vararg = 1;
                last;
            }
            push @params, $self->_expect_name;
            if ($self->_check(',')) {
                $self->_advance;
                next;
            }
            last;
        }
    }
    $self->_expect(')');
    my $body = $self->_parse_block;
    $self->_expect_kw('end');
    return { params => \@params, vararg => $vararg, body => $body };
}

sub _parse_local {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    $self->_expect_kw('local');

    if ($self->_check_kw('function')) {
        $self->_advance;
        my $name = $self->_expect_name;
        my $body = $self->_parse_funcbody(0);
        return { type => 'localfunction', name => $name, body => $body, line => $line };
    }

    my @names = ($self->_expect_name);
    while ($self->_check(',')) {
        $self->_advance;
        push @names, $self->_expect_name;
    }
    my $exprs = [];
    if ($self->_check('=')) {
        $self->_advance;
        $exprs = $self->_parse_explist;
    }
    return { type => 'local', names => \@names, exprs => $exprs, line => $line };
}

# varlist '=' explist, or a bare functioncall used as a statement.
sub _parse_exprstat {
    my ($self) = @_;
    my $line = $self->_cur->{line};
    my $first = $self->_parse_suffixedexp;

    if ($self->_check('=') || $self->_check(',')) {
        my @targets = ($first);
        while ($self->_check(',')) {
            $self->_advance;
            push @targets, $self->_parse_suffixedexp;
        }
        for my $t (@targets) {
            $self->_error('cannot assign to this expression')
                unless $t->{type} eq 'name' || $t->{type} eq 'index';
        }
        $self->_expect('=');
        my $exprs = $self->_parse_explist;
        return { type => 'assign', targets => \@targets, exprs => $exprs, line => $line };
    }

    $self->_error('syntax error: expression used as a statement')
        unless $first->{type} eq 'call' || $first->{type} eq 'methodcall';
    return { type => 'callstat', call => $first, line => $line };
}

# --- expressions ---------------------------------------------------------

sub _is_unop {
    my ($self) = @_;
    my $t = $self->_cur;
    return 1 if $t->{type} eq '-' || $t->{type} eq '#';
    return 1 if $t->{type} eq 'keyword' && $t->{value} eq 'not';
    return 0;
}

sub _binop_key {
    my ($self) = @_;
    my $t = $self->_cur;
    if ($t->{type} eq 'keyword') {
        return $t->{value} if $t->{value} eq 'and' || $t->{value} eq 'or';
        return undef;
    }
    return $t->{type};
}

sub _parse_expr {
    my ($self, $limit) = @_;
    my $left;

    if ($self->_is_unop) {
        my $t = $self->_advance;
        my $op = $t->{type} eq 'keyword' ? $t->{value} : $t->{type};
        my $operand = $self->_parse_expr($UNARY_PREC);
        $left = { type => 'unop', op => $op, expr => $operand, line => $t->{line} };
    }
    else {
        $left = $self->_parse_simple_expr;
    }

    while (1) {
        my $key = $self->_binop_key;
        last unless defined $key && exists $BINOP{$key};
        my ($lprec, $rprec) = @{ $BINOP{$key} };
        last if $lprec <= $limit;
        my $line = $self->_cur->{line};
        $self->_advance;
        my $right = $self->_parse_expr($rprec);
        $left = { type => 'binop', op => $key, lhs => $left, rhs => $right, line => $line };
    }
    return $left;
}

sub _parse_simple_expr {
    my ($self) = @_;
    my $t = $self->_cur;

    if ($t->{type} eq 'number') { $self->_advance; return { type => 'number', value => $t->{value} }; }
    if ($t->{type} eq 'string') { $self->_advance; return { type => 'string', value => $t->{value} }; }
    if ($t->{type} eq '...')    { $self->_advance; return { type => 'vararg' }; }
    if ($t->{type} eq '{')      { return $self->_parse_table; }

    if ($t->{type} eq 'keyword') {
        if ($t->{value} eq 'nil')   { $self->_advance; return { type => 'nil' }; }
        if ($t->{value} eq 'true')  { $self->_advance; return { type => 'true' }; }
        if ($t->{value} eq 'false') { $self->_advance; return { type => 'false' }; }
        if ($t->{value} eq 'function') {
            $self->_advance;
            my $body = $self->_parse_funcbody(0);
            return { type => 'function', body => $body };
        }
    }

    if ($t->{type} eq 'name' || $t->{type} eq '(') {
        return $self->_parse_suffixedexp;
    }

    $self->_error('unexpected token in expression');
}

sub _parse_primaryexp {
    my ($self) = @_;
    if ($self->_check('name')) {
        return { type => 'name', name => $self->_advance->{value} };
    }
    if ($self->_check('(')) {
        $self->_advance;
        my $e = $self->_parse_expr(0);
        $self->_expect(')');
        return { type => 'paren', expr => $e };
    }
    $self->_error("expected identifier or '('");
}

# prefixexp with the full suffix chain: .Name, [exp], :Name args, args.
sub _parse_suffixedexp {
    my ($self) = @_;
    my $base = $self->_parse_primaryexp;
    while (1) {
        my $line = $self->_cur->{line};
        if ($self->_check('.')) {
            $self->_advance;
            my $field = $self->_expect_name;
            $base = { type => 'index', obj => $base, key => { type => 'string', value => $field }, line => $line };
        }
        elsif ($self->_check('[')) {
            $self->_advance;
            my $key = $self->_parse_expr(0);
            $self->_expect(']');
            $base = { type => 'index', obj => $base, key => $key, line => $line };
        }
        elsif ($self->_check(':')) {
            $self->_advance;
            my $method = $self->_expect_name;
            my $args = $self->_parse_args;
            $base = { type => 'methodcall', obj => $base, method => $method, args => $args, line => $line };
        }
        elsif ($self->_check('(') || $self->_check('string') || $self->_check('{')) {
            my $args = $self->_parse_args;
            $base = { type => 'call', func => $base, args => $args, line => $line };
        }
        else {
            last;
        }
    }
    return $base;
}

sub _parse_args {
    my ($self) = @_;
    if ($self->_check('(')) {
        $self->_advance;
        my $args = $self->_check(')') ? [] : $self->_parse_explist;
        $self->_expect(')');
        return $args;
    }
    if ($self->_check('string')) {
        return [ { type => 'string', value => $self->_advance->{value} } ];
    }
    if ($self->_check('{')) {
        return [ $self->_parse_table ];
    }
    $self->_error('expected function arguments');
}

sub _parse_table {
    my ($self) = @_;
    $self->_expect('{');
    my @fields;
    while (!$self->_check('}')) {
        if ($self->_check('[')) {
            $self->_advance;
            my $key = $self->_parse_expr(0);
            $self->_expect(']');
            $self->_expect('=');
            my $value = $self->_parse_expr(0);
            push @fields, { type => 'keyed', key => $key, value => $value };
        }
        elsif ($self->_check('name') && $self->_cur(1)->{type} eq '=') {
            my $name = $self->_advance->{value};
            $self->_advance; # '='
            my $value = $self->_parse_expr(0);
            push @fields, { type => 'named', key => $name, value => $value };
        }
        else {
            my $value = $self->_parse_expr(0);
            push @fields, { type => 'pos', value => $value };
        }

        if ($self->_check(',') || $self->_check(';')) {
            $self->_advance;
        }
        else {
            last;
        }
    }
    $self->_expect('}');
    return { type => 'table', fields => \@fields };
}

sub _parse_explist {
    my ($self) = @_;
    my @exprs = ($self->_parse_expr(0));
    while ($self->_check(',')) {
        $self->_advance;
        push @exprs, $self->_parse_expr(0);
    }
    return \@exprs;
}

1;
