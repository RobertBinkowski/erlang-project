-module(routing).
-export([route/2, update_routing_table/3, get_route_pid/2, update_neighbor_routing_table/5]).
-include("header.hrl").

route(Node, Msg) ->
    case Msg of
        {compute_nth_prime, _N, DestinationNickname, _SenderNickname, _Hops} ->
            case get_route_pid(Node, DestinationNickname) of
                {NeighborNickname, NeighborPid} ->
                    NeighborPid ! Msg,
                    update_routing_table(Node, DestinationNickname, NeighborNickname);
                false ->
                    ok
            end;
        {receive_answer, _N, _M, DestinationNickname, _SenderNickname, _Hops} ->
            case get_route_pid(Node, DestinationNickname) of
                {NeighborNickname, NeighborPid} ->
                    NeighborPid ! Msg,
                    update_routing_table(Node, DestinationNickname, NeighborNickname);
                false ->
                    ok
            end
    end.

update_routing_table(Node, DestinationNickname, NeighborNickname) ->
    case lists:keyfind(DestinationNickname, 1, Node#node.routing_table) of
        {DestinationNickname, _, _Hops} ->
            Node;
        false ->
            NewNode = Node#node{
                routing_table =
                    Node#node.routing_table ++ [{DestinationNickname, NeighborNickname, 1}]
            },
            NeighborPid = get_route_pid(NewNode, NeighborNickname),
            update_neighbor_routing_table(
                NewNode, DestinationNickname, NeighborNickname, NeighborPid, 1
            )
    end.

get_route_pid(Node, Nickname) ->
    case lists:keyfind(Nickname, 1, Node#node.routing_table) of
        {Nickname, Neighbor, _Hops} ->
            lists:keyfind(Neighbor, 1, Node#node.neighbors);
        false ->
            false
    end.

update_neighbor_routing_table(Node, DestinationNickname, NeighborNickname, NeighborPid, Hops) ->
    case Hops < ?MAX_HOPS of
        true ->
            case lists:keyfind(NeighborNickname, 1, Node#node.routing_table) of
                {NeighborNickname, _, _} ->
                    ok;
                false ->
                    NewNode = Node#node{
                        routing_table =
                            Node#node.routing_table ++ [{NeighborNickname, NeighborNickname, Hops}]
                    },
                    NeighborPid !
                        {update_neighbor_routing_table, DestinationNickname, Node#node.nickname,
                            Hops + 1},
                    % Send a message to update the node state with the new routing table
                    self() ! {update_state, NewNode}
            end;
        false ->
            ok
    end.
