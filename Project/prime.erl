-module(prime).
-include("header.hrl").
-export([is_prime/1, nth_prime/1, next_prime/1]).

% This function checks if a given number is prime
is_prime(2) ->
    true;
is_prime(3) ->
    true;
is_prime(N) when N rem 2 =:= 0 -> false;
is_prime(N) ->
    MaxDivisor = round(math:sqrt(N)),
    not lists:any(fun(Div) -> N rem Div =:= 0 end, lists:seq(3, MaxDivisor, 2)).

% This function returns the nth prime number
nth_prime(N) when N > 0 ->
    nth_prime(N, 3, 2).

nth_prime(N, Prime, Count) ->
    case is_prime(Prime) of
        true ->
            if
                Count =:= N ->
                    Prime;
                true ->
                    NextPrime = next_prime(Prime + 2),
                    nth_prime(N, NextPrime, Count + 1)
            end;
        false ->
            NextPrime = next_prime(Prime + 2),
            nth_prime(N, NextPrime, Count)
    end.

% This function returns the smallest prime number larger than X
next_prime(X) ->
    case is_prime(X) of
        true -> X;
        false -> next_prime(X + 2)
    end.
