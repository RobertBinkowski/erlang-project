%%%-------------------------------------------------------------------
%%% @author Robert Binkowski - C00237917
%%% @copyright (C) 2022, Robert Binkowski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2022 by Robert Binkowski
%%%-------------------------------------------------------------------
-module(project).
-include_lib("eunit/include/eunit.hrl").
-export([start/0, rpc/2, launchNode/1, print/1, serve/1, nth/1]).

start()->
    %This section creates and assigns the nodes
    Node1 = launchNode(node1),
    Node2 = launchNode(node2),
    Node3 = launchNode(node3),
    Node4 = launchNode(node4),
    Node5 = launchNode(node5),
    end.

% Call other process on the network
% rpc(Pid, Message) ->
%     Pid ! {self(), Message},
%     receive
%         {Pid, Reply} ->
%             Reply
%     end.
% Launch Node
launchNode(Nickname) ->
    Pid = spawn(project, node, [name, [],_]),
    register(Nickname, Pid),
    Pid.
% serve(State)->
%     receive
%         {Sender, Data}->

listTable([H|T], Tail)->
    [H|listTable(T,Tail)];
listTable([],Tail)->
    Tail.

node(Name, NeighbourList, Table)->
    receive
        {computeNthPrime,N,DestinationNickname,SenderNickname,Hoops}->

        {receiveAnswer, N, M, DestinationNickname, SenderNickname,Hops}->

    end.

computeNthPrime(N,DestinationNickname,SenderNickname,Hoops)->

receiveAnswer(N, M, DestinationNickname, SenderNickname,Hops)->
    M = nth(N).

print(Pid)->
    io:format(("~p" ,Pid)).

% Figure out the nth prime
nth([])->
    [];
nth([H|T])->
    List = lists.filter(fun(N) -> rem H /= 0 end, T),
    [H|nth(List)];
nth(N)->
    nth(Lists: seq(2,N)).
