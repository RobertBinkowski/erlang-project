%%%-------------------------------------------------------------------
%%% @author Robert Binkowski - C00237917
%%% @copyright (C) 2022, Robert Binkowski
%%%-------------------------------------------------------------------
-module(project).
-export([start/0]).

% max number of hops in RIP
-define(HOPS, 15).

start() ->
    P = {baby, 24, {junior, 2333}},
    io:fwrite("~w", [tuple_size(P)]).

loop() ->
    receive
        {computeNthPrime, N, DestinationNickname, SenderNickname, Hops} ->
            io:fwrite("Computer, Nth, Prime\n");
        {receiveAnswer, N, M, DestinationNickname, SenderNickname, Hops} ->
            io:fwrite("Receive, Answer\n");
        %Empty Request Error
        _ ->
            {error, bad_request}
    end.

% Create A node
launchNode(Nickname) ->
    Pid = spawn(project, node, [name, []]),
    register(Nickname, Pid),
    Pid.

% Connect Nodes
connectNode(NicknameOne, PidOne, NicknameTwo, PidTwo) ->
    io:fwrite("Connect, Node\n").

% Print Table of PID's
printTable(Pid) ->
    io:fwrite("Print, Table\n").
