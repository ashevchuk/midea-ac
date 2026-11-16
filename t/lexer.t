#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../reverse/lua";

use PureLua::Lexer;

sub types_of {
    my ($tokens) = @_;
    return [ map { $_->{type} } @$tokens ];
}

sub tok { return { type => $_[0], value => $_[1] } };

sub strip_lines {
    my ($tokens) = @_;
    return [ map { { type => $_->{type}, value => $_->{value} } } @$tokens ];
}

# 1. keywords/operators, including a name that starts with a keyword
# ("forever" must not lex as keyword 'for' + name 'ever').
{
    my $tokens = PureLua::Lexer->tokenize('if a == 1 then forever = true end');
    is_deeply(
        strip_lines($tokens),
        [
            tok('keyword', 'if'),
            tok('name',    'a'),
            tok('==',      '=='),
            tok('number',  1),
            tok('keyword', 'then'),
            tok('name',    'forever'),
            tok('=',       '='),
            tok('keyword', 'true'),
            tok('keyword', 'end'),
            tok('eof',     undef),
        ],
        'keywords, operators, and a keyword-prefixed name'
    );
}

# 2. numbers: decimal, float, exponent, hex.
{
    my $tokens = PureLua::Lexer->tokenize('42 3.14 1e10 0xFF .5');
    is_deeply(
        strip_lines($tokens),
        [
            tok('number', 42),
            tok('number', 3.14),
            tok('number', 1e10),
            tok('number', 255),
            tok('number', 0.5),
            tok('eof',    undef),
        ],
        'decimal, float, exponent and hex numbers'
    );
}

# 3. strings with escapes, both quote styles.
{
    my $tokens = PureLua::Lexer->tokenize(q{'hello\nworld' "a\"b" '\65\66'});
    is_deeply(
        strip_lines($tokens),
        [
            tok('string', "hello\nworld"),
            tok('string', 'a"b'),
            tok('string', 'AB'),
            tok('eof',    undef),
        ],
        'short strings with escapes'
    );
}

# 4. long strings and long/line comments, interleaved with real code.
{
    my $src = "--[[ a comment\nspanning lines ]]\nx = [==[ raw ]==]\n-- trailing\ny = 1";
    my $tokens = PureLua::Lexer->tokenize($src);
    is_deeply(
        strip_lines($tokens),
        [
            tok('name',   'x'),
            tok('=',      '='),
            tok('string', ' raw '),
            tok('name',   'y'),
            tok('=',      '='),
            tok('number', 1),
            tok('eof',    undef),
        ],
        'long comments, long strings, and line comments are skipped/decoded correctly'
    );

    # Line tracking: 'x' sits after a two-line long comment, so it starts
    # on line 3; 'y' sits two lines further down.
    is($tokens->[0]{line}, 3, 'line counter advances across a multi-line long comment');
    is($tokens->[3]{line}, 5, 'line counter advances across the long string and trailing comment');
}

# 5. concatenation vs varargs vs dot, longest-match-first.
{
    my $tokens = PureLua::Lexer->tokenize('a..b .. c...');
    is_deeply(
        types_of($tokens),
        [ 'name', '..', 'name', '..', 'name', '...', 'eof' ],
        'longest-match-first distinguishes . / .. / ...'
    );
}

# 6. a representative snippet shaped like the real device scripts.
{
    my $tokens = PureLua::Lexer->tokenize(<<'LUA');
local bit = require "bit"
local function splitStrByChar(str, sepChar)
    local t = {}
    for s in string.gmatch(str, '[^' .. sepChar .. ']+') do
        table.insert(t, s)
    end
    return t
end
LUA
    ok(scalar(@$tokens) > 10, 'tokenizes a closure/require/gmatch-shaped snippet without dying');
    is($tokens->[-1]{type}, 'eof', 'ends in eof');
}

done_testing;
