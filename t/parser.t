#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../reverse/lua";

use PureLua::Parser;

sub parse_expr_str {
    my ($src) = @_;
    my $ast = PureLua::Parser->parse("return $src");
    return $ast->{body}[0]{exprs}[0];
}

# 1. local / assignment.
{
    my $ast = PureLua::Parser->parse('local a, b = 1, 2');
    is_deeply(
        $ast->{body}[0],
        {
            type  => 'local',
            names => [ 'a', 'b' ],
            exprs => [ { type => 'number', value => 1 }, { type => 'number', value => 2 } ],
            line  => 1,
        },
        'local with multiple names/exprs'
    );
}

# 2. if/elseif/else.
{
    my $ast = PureLua::Parser->parse('if a then x = 1 elseif b then x = 2 else x = 3 end');
    my $stmt = $ast->{body}[0];
    is($stmt->{type}, 'if', 'parses as if-statement');
    is(scalar(@{ $stmt->{clauses} }), 2, 'if + one elseif = two clauses');
    is($stmt->{clauses}[0]{cond}{name}, 'a', 'first clause condition is `a`');
    is($stmt->{clauses}[1]{cond}{name}, 'b', 'second clause condition is `b`');
    ok(defined $stmt->{else_body}, 'else body present');
}

# 3. numeric for and generic for.
{
    my $ast = PureLua::Parser->parse('for i = 1, 10, 2 do end for k, v in pairs(t) do end');
    is($ast->{body}[0]{type}, 'fornum', 'numeric for');
    is($ast->{body}[0]{var}, 'i', 'numeric for loop variable');
    ok(defined $ast->{body}[0]{step}, 'numeric for has a step expression');

    is($ast->{body}[1]{type}, 'forin', 'generic for');
    is_deeply($ast->{body}[1]{names}, [ 'k', 'v' ], 'generic for names');
}

# 4. method-call function statement implicitly adds 'self'.
{
    my $ast = PureLua::Parser->parse('function obj:method(x) end');
    my $stmt = $ast->{body}[0];
    is($stmt->{type}, 'function', 'function statement');
    is_deeply($stmt->{body}{params}, [ 'self', 'x' ], 'method function gets an implicit self param');
    is($stmt->{target}{type}, 'index', 'target is obj.method as an index node');
    is($stmt->{target}{key}{value}, 'method', 'index key is the method name');
}

# 5. table constructor: positional, named, and keyed fields together.
{
    my $tbl = parse_expr_str('{1, 2, x = 3, [4+1] = 5}');
    is($tbl->{type}, 'table', 'table constructor');
    is_deeply(
        [ map { $_->{type} } @{ $tbl->{fields} } ],
        [ 'pos', 'pos', 'named', 'keyed' ],
        'field kinds in source order'
    );
    is($tbl->{fields}[2]{key}, 'x', 'named field key');
    is($tbl->{fields}[3]{key}{type}, 'binop', 'keyed field key is itself an expression');
}

# 6. operator precedence and associativity - the part most likely to be
# subtly wrong, so pin down exact shapes.
{
    # 1 + 2 * 3 ^ 2  =>  1 + (2 * (3 ^ 2))
    my $e = parse_expr_str('1 + 2 * 3 ^ 2');
    is($e->{op}, '+', 'top operator is +');
    is($e->{rhs}{op}, '*', '+ has a * on its right');
    is($e->{rhs}{rhs}{op}, '^', '* has a ^ on its right (tighter than *)');

    # -2 ^ 2  =>  -(2 ^ 2): ^ binds tighter than unary minus.
    my $u = parse_expr_str('-2 ^ 2');
    is($u->{type}, 'unop', 'unary minus is the outermost node');
    is($u->{expr}{type}, 'binop', 'operand of unary minus is a binop');
    is($u->{expr}{op}, '^', 'that binop is ^, not already-negated 2');

    # a .. b .. c  =>  a .. (b .. c): right-associative.
    my $cat = parse_expr_str('a .. b .. c');
    is($cat->{op}, '..', 'top-level ..');
    is($cat->{lhs}{type}, 'name', 'left side of top .. is a plain name (right-assoc)');
    is($cat->{rhs}{op}, '..', 'right side of top .. is itself a .. node');

    # a or b and c  =>  a or (b and c): `and` binds tighter than `or`.
    my $logic = parse_expr_str('a or b and c');
    is($logic->{op}, 'or', 'top-level or');
    is($logic->{rhs}{op}, 'and', '`and` nests under `or`, not the other way round');
}

# 7. chained index/method-call suffixes.
{
    my $e = parse_expr_str('a.b.c:d(1, 2)');
    is($e->{type}, 'methodcall', 'outermost node is the method call');
    is($e->{method}, 'd', 'method name');
    is(scalar(@{ $e->{args} }), 2, 'two call arguments');
    is($e->{obj}{type}, 'index', 'receiver is an index chain');
    is($e->{obj}{key}{value}, 'c', 'innermost index key is c');
    is($e->{obj}{obj}{key}{value}, 'b', 'next index key out is b');
    is($e->{obj}{obj}{obj}{name}, 'a', 'base name is a');
}

# 8. a snippet shaped like the real device scripts (closures sharing an
# upvalue table, string.gmatch with a built pattern, bit library calls).
{
    my $ast = PureLua::Parser->parse(<<'LUA');
local keyT = {}
local function splitStrByChar(str, sepChar)
    local t = {}
    for s in string.gmatch(str, '[^' .. sepChar .. ']+') do
        table.insert(t, s)
    end
    return t
end
local function jsonToData(jsonStr)
    local v = bit.band(keyT.POWER, 0xff)
    return v
end
LUA
    is(scalar(@{ $ast->{body} }), 3, 'three top-level statements');
    is($ast->{body}[1]{type}, 'localfunction', 'local function statement');
    is($ast->{body}[1]{body}{params}[0], 'str', 'first param name');
}

done_testing;
