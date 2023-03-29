-module(main).
-export([
    launch_node/1,
    print_table/1,
    display_result/0
]).

-include("header.hrl").

launch_node(Nickname) ->
    spawn(fun() ->
        node:node_loop(#node{nickname = Nickname, neighbors = [], routing_table = []})
    end).

print_table(Pid) ->
    Pid ! {print_table, self()},
    receive
        {print_table, Table} ->
            io:format("Routing table: ~p~n", [Table])
    end.

display_result() ->
    receive
        {receive_answer, N, M, _DestinationNickname, Hops} ->
            io:format("The ~p-th prime number is ~p (Hops: ~p).~n", [N, M, Hops])
    after 10000 ->
        io:format("No answer received.~n")
    end.
