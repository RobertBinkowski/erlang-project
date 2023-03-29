-module(node).
-export([node_loop/1, handle_update_routing_table/4, get_pid/2]).
-include("header.hrl").

node_loop(Node) ->
    receive
        {connect, _NewNeighbor} ->
            node_loop(Node#node{neighbors = Node#node.neighbors ++ [_NewNeighbor]});
        {print_table, Pid} ->
            Pid ! {print_table, Node#node.routing_table},
            node_loop(Node);
        {compute_nth_prime, N, DestinationNickname, SenderNickname, Hops} ->
            routing:route(Node, {compute_nth_prime, N, DestinationNickname, SenderNickname, Hops}),
            node_loop(Node);
        {receive_answer, N, M, DestinationNickname, SenderNickname, Hops} ->
            routing:route(Node, {receive_answer, N, M, DestinationNickname, SenderNickname, Hops}),
            node_loop(Node);
        {update_routing_table, DestinationNickname, SenderNickname, Hops} ->
            NewNode = handle_update_routing_table(Node, DestinationNickname, SenderNickname, Hops),
            node_loop(NewNode);
        {update_state, NewNode} ->
            node_loop(NewNode);
        _ ->
            node_loop(Node)
    end.

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
