-module(routing).
-export([forward_packet/2, compute_nth_prime/5, receive_answer/6]).
-include("../include/header.hrl").

forward_packet(Node, Packet) ->
    case Packet#packet.hops > ?MAX_HOPS of
        % Discard the packet if it reached the max number of hops
        true ->
            ok;
        false ->
            NextHop = node:get_next_hop(Node, Packet#packet.destination_nickname),
            case NextHop of
                none ->
                    ok;
                NextHop ->
                    NewPacket = Packet#packet{hops = Packet#packet.hops + 1},
                    NextHop ! NewPacket
            end
    end.

compute_nth_prime(Node, N, DestinationNickname, SenderNickname, Hops) ->
    case Node#node.nickname == DestinationNickname of
        true ->
            M = prime:computeNthPrime(N),
            receive_answer(Node, N, M, SenderNickname, DestinationNickname, Hops);
        false ->
            Packet = #packet{
                type = computeNthPrime,
                sender_nickname = SenderNickname,
                destination_nickname = DestinationNickname,
                payload = {computeNthPrime, N},
                hops = Hops
            },
            forward_packet(Node, Packet)
    end.

receive_answer(Node, N, M, DestinationNickname, SenderNickname, Hops) ->
    case Node#node.nickname == DestinationNickname of
        true ->
            io:format("The ~Bth prime number is ~B~n", [N, M]);
        false ->
            Packet = #packet{
                type = receiveAnswer,
                sender_nickname = SenderNickname,
                destination_nickname = DestinationNickname,
                payload = {receiveAnswer, N, M},
                hops = Hops
            },
            forward_packet(Node, Packet)
    end.
