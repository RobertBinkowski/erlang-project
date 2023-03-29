-module(project).
-export([
    launch_node/1,
    connect_node/4,
    print_table/1,
    compute_nth_prime/4,
    display_result/0
]).

-include("header.hrl").

launch_node(Nickname) ->
    spawn(fun() ->
        node:node_loop(#node{nickname = Nickname, neighbors = [], routing_table = []})
    end).

connect_node(NicknameOne, PidOne, NicknameTwo, PidTwo) ->
    PidOne ! {connect, {NicknameTwo, PidTwo}},
    PidTwo ! {connect, {NicknameOne, PidOne}}.

print_table(Pid) ->
    Pid ! {print_table, self()},
    receive
        {print_table, Table} ->
            io:format("Routing table: ~p~n", [Table])
    end.

compute_nth_prime(Pid, N, DestinationNickname, SenderNickname) ->
    Pid ! {compute_nth_prime, N, DestinationNickname, SenderNickname, 0}.

display_result() ->
    receive
        {receive_answer, N, M} ->
            io:format("The ~p-th prime number is ~p.~n", [N, M])
    after 10000 ->
        io:format("No answer received.~n")
    end.
