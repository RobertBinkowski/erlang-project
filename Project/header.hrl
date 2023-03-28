%% header.hrl
%% Header file for the prime module

%% is_prime/1
%% @doc Checks if a given number is prime
-spec is_prime(integer()) -> boolean().

%% nth_prime/1
%% @doc Returns the nth prime number
-spec nth_prime(pos_integer()) -> pos_integer().

%% next_prime/1
%% @doc Returns the smallest prime number larger than X
-spec next_prime(pos_integer()) -> pos_integer().
