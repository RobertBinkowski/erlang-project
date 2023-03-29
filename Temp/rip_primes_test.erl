-module(rip_primes_test).
-include_lib("eunit/include/eunit.hrl").

%%% Utility functions
prime(N) ->
    prime(N, 2, 1).

prime(N, _, Acc) when N < 2 ->
    prime_not_found;
prime(N, N, Acc) ->
    Acc;
prime(N, Divisor, Acc) ->
    case N rem Divisor of
        0 -> prime_not_found;
        _ -> prime(N, Divisor + 1, Acc)
    end.

nth_prime(N) ->
    nth_prime(N, 2, 0).

nth_prime(0, _, Acc) ->
    Acc;
nth_prime(N, Current, Acc) ->
    case prime(Current) of
        prime_not_found ->
            nth_prime(N, Current + 1, Acc);
        _ ->
            nth_prime(N - 1, Current + 1, Acc + 1)
    end.

%%% Tests
setup_nodes() ->
    Pid1 = rip_primes:launchNode("alice"),
    Pid2 = rip_primes:launchNode("bob"),
    Pid3 = rip_primes:launchNode("carol"),
    Pid4 = rip_primes:launchNode("dave"),
    ok = rip_primes:connectNode("alice", Pid1, "bob", Pid2),
    ok = rip_primes:connectNode("alice", Pid1, "carol", Pid3),
    ok = rip_primes:connectNode("carol", Pid3, "dave", Pid4),
    {Pid1, Pid2, Pid3, Pid4}.

route_table_test_() ->
    {setup, fun setup_nodes/0, fun(_) -> ok end, [
        {"Node1 Routing Table", fun(Node1) -> rip_primes:printTable(Node1) end},
        {"Node2 Routing Table", fun(Node2) -> rip_primes:printTable(Node2) end},
        {"Node3 Routing Table", fun(Node3) -> rip_primes:printTable(Node3) end},
        {"Node4 Routing Table", fun(Node4) -> rip_primes:printTable(Node4) end}
    ]}.

compute_nth_prime_test() ->
    {Pid1, Pid2, Pid3, Pid4} = setup_nodes(),
    5 = rip_primes:computeNthPrime(3, "alice", "dave"),
    11 = rip_primes:computeNthPrime(5, "alice", "carol"),
    29 = rip_primes:computeNthPrime(10, "bob", "alice"),
    89 = rip_primes:computeNthPrime(24, "carol", "dave").

receive_answer_test() ->
    {Pid1, Pid2, Pid3, Pid4} = setup_nodes(),
    ok = rip_primes:receiveAnswer(3, 5, "alice", "dave"),
    ok = rip_primes:receiveAnswer(5, 11, "alice", "carol"),
    ok = rip_primes:receiveAnswer(10, 29, "bob", "alice"),
    ok = rip_primes:receiveAnswer(24, 89, "carol", "dave").
