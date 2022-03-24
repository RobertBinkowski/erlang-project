%%%-------------------------------------------------------------------
%%% @author Robert Binkowski - C00237917
%%% @copyright (C) 2022, Robert Binkowski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2022 by Robert Binkowski
%%%-------------------------------------------------------------------
-module(project).
-export([]).

rpc(Pid, Message)->
    Pid!{self(), Message},
    receive
        {Pid, Reply}->
            Reply
        end

launchNode(Nickname)->
    Pid = spawn(project, print, [pid]),
    register(Nickname, Pid),
    Pid.

print(Print)->
    io:format(("~p ", Print)).

serve(State)->
    receive
        {Sender, Data}->
            Answer=