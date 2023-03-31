-module(prime).
-include("../include/header.hrl").
-export([is_prime/1, computeNthPrime/1, next_prime/1]).

% This function checks if a given number is prime
is_prime(2) ->
    true;
is_prime(3) ->
    true;
% Check if N is even
is_prime(N) when N rem 2 =:= 0 -> false;
is_prime(N) ->
    % Calculate the highest divider of N
    MaxDivisor = round(math:sqrt(N)),
    % Check all dividers from 3 up to N without any remainders
    not lists:any(fun(Div) -> N rem Div =:= 0 end, lists:seq(3, MaxDivisor, 2)).

% Returns the nth prime number
computeNthPrime(N) when N > 0 ->
    computeNthPrime(N, 3, 2).
computeNthPrime(N, Prime, Count) ->
    case is_prime(Prime) of
        true ->
            % If the current number is prime and is the nth prime, return it
            if
                Count =:= N ->
                    Prime;
                % Calculate the next odd number larger than Prime
                true ->
                    % Calculate the next odd number larger than Prime
                    NextPrime = next_prime(Prime + 2),
                    % Recursively call computeNthPrime with the next prime and an incremented count
                    computeNthPrime(N, NextPrime, Count + 1)
            end;
        false ->
            % If the number is not prime continue the search
            NextPrime = next_prime(Prime + 2),
            % Recursively call computeNthPrime with the next prime and the same count
            computeNthPrime(N, NextPrime, Count)
    end.

% This function returns the smallest prime number larger than X
next_prime(X) ->
    case is_prime(X) of
        true -> X;
        % Recursively call computeNthPrime with the next prime and the same count
        false -> next_prime(X + 2)
    end.
