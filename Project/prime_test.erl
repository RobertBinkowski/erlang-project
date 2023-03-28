%% prime_tests.erl
-module(prime_test).
-include_lib("eunit/include/eunit.hrl").
-include("header.hrl").

%% Test cases for is_prime/1
is_prime_test_() ->
    [
        ?_assertEqual(true, prime:is_prime(2)),
        ?_assertEqual(true, prime:is_prime(3)),
        ?_assertEqual(false, prime:is_prime(4)),
        ?_assertEqual(true, prime:is_prime(5)),
        ?_assertEqual(false, prime:is_prime(6)),
        ?_assertEqual(true, prime:is_prime(7)),
        ?_assertEqual(false, prime:is_prime(8)),
        ?_assertEqual(false, prime:is_prime(9)),
        ?_assertEqual(false, prime:is_prime(10)),
        ?_assertEqual(true, prime:is_prime(11))
    ].

%% Test cases for nth_prime/1
nth_prime_test_() ->
    [
        ?_assertEqual(2, prime:nth_prime(1)),
        ?_assertEqual(3, prime:nth_prime(2)),
        ?_assertEqual(5, prime:nth_prime(3)),
        ?_assertEqual(7, prime:nth_prime(4)),
        ?_assertEqual(11, prime:nth_prime(5)),
        ?_assertEqual(13, prime:nth_prime(6)),
        ?_assertEqual(17, prime:nth_prime(7)),
        ?_assertEqual(19, prime:nth_prime(8)),
        ?_assertEqual(23, prime:nth_prime(9)),
        ?_assertEqual(29, prime:nth_prime(10))
    ].

%% Test cases for next_prime/1
next_prime_test_() ->
    [
        ?_assertEqual(2, prime:next_prime(1)),
        ?_assertEqual(3, prime:next_prime(2)),
        ?_assertEqual(5, prime:next_prime(3)),
        ?_assertEqual(7, prime:next_prime(5)),
        ?_assertEqual(11, prime:next_prime(9)),
        ?_assertEqual(13, prime:next_prime(11)),
        ?_assertEqual(17, prime:next_prime(15)),
        ?_assertEqual(19, prime:next_prime(17)),
        ?_assertEqual(23, prime:next_prime(21)),
        ?_assertEqual(29, prime:next_prime(27))
    ].
