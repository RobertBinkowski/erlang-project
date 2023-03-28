-module(node).
-export([node_loop/1, handle_update_routing_table/4, get_pid/2]).

-include("project.erl").

% Main loop of a node, handling incoming messages.
% It responds to connection requests, print table requests,
% prime number computation requests, and answer receiving requests.
node_loop(Node) ->
    receive
        %%% When a connection request is received, update the neighbors list.
        {connect, {Nickname, Pid}} ->
            node_loop(Node#node{neighbors = Node#node.neighbors ++ [{Nickname, Pid}]});
        %%% When a print table request is received, send back the routing table.
        {print_table, Pid} ->
            Pid ! {print_table, Node#node.routing_table},
            node_loop(Node);
        %%% When a prime computation request is received, either process it or route it.
        {compute_nth_prime, N, DestinationNickname, SenderNickname, Hops} ->
            routing:route(Node, {compute_nth_prime, N, DestinationNickname, SenderNickname, Hops}),
            node_loop(Node);
        %%% When an answer is received, either display it or route it.
        {receive_answer, N, M, DestinationNickname, SenderNickname, Hops} ->
            routing:route(Node, {receive_answer, N, M, DestinationNickname, SenderNickname, Hops}),
            node_loop(Node);
        %%% When an update_routing_table request is received, handle it.
        {update_routing_table, DestinationNickname, SenderNickname, Hops} ->
            NewNode = handle_update_routing_table(Node, DestinationNickname, SenderNickname, Hops),
            node_loop(NewNode);
        %%% Ignore any other messages.
        _ ->
            node_loop(Node)
    end.

% Handles the update_routing_table message in the node loop.
handle_update_routing_table(Node, DestinationNickname, SenderNickname, Hops) ->
    case lists:keyfind(DestinationNickname, 1, Node#node.routing_table) of
        {DestinationNickname, _, OldHops} when OldHops =< Hops ->
            Node;
        _ ->
            NewNode = Node#node{
                routing_table =
                    Node#node.routing_table ++ [{DestinationNickname, SenderNickname, Hops}]
            },
            routing:update_neighbor_routing_table(
                NewNode,
                DestinationNickname,
                SenderNickname,
                get_pid(NewNode, SenderNickname),
                Hops + 1
            )
    end.

get_pid(Node, Nickname) ->
    case lists:keyfind(Nickname, 1, Node#node.neighbors) of
        {Nickname, Pid} -> Pid;
        false -> false
    end.
