%%%-------------------------------------------------------------------
%%% @author Joseph Kehoe <joe@pop-os>
%%% @copyright (C) 2022, Joseph Kehoe
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2022 by Joseph Kehoe <joe@pop-os>
%%%-------------------------------------------------------------------
-module(simpleServer).
-export([fib/1,fibServer/0,fibCountServer/1,fibsAreUs/1,doFibs/1]).

fib(0)->
    1;
fib(1) ->
    1;
fib(N) ->
    fib(N-1)+fib(N-2).

fibServer()->
    receive
        N when is_number(N) ->
            X=fib(N),
            io:format("Answer is:~p~n",[X]),
            fibServer();
        {done,Name}->
            io:format("ok, I will finish ~p~n",[Name])
    end.









fibCountServer(Count)->
    receive
        N when is_number(N) ->
            X=fib(N),
            io:format("Answer ~p is:~p~n",[Count,X]),
            fibCountServer(Count+1);
        {done,Name}->
            io:format("ok, I will finish ~p~n",[Name])
    end.


















fibsAreUs(Count)->
    receive
        {Sender,num,N} ->
            X=fib(N),
            Sender!{self(),X,Count},
            fibsAreUs(Count+1);
        {done,Name}->
            io:format("ok, I will finish ~p~n",[Name])
    end.

doFibs([FibsPID,Cost])->
    receive
        N when is_number(N) ->
            FibsPID!{self(),num,N},
            receive
                {FibsPID,X,Count} ->
                    ok
            end,
            io:format("Answer is:~p~n",[X]),
            doFibs([FibsPID,Cost+X]);
        {done,Name}->
            io:format("ok, ~p I will finish with bill of:~p~n",[Name,Cost]),
            FibsPID!{done,doFibs}
    end.
