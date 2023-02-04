%%%-------------------------------------------------------------------
%%% @author Joseph Kehoe <joe@pop-os>
%%% @copyright (C) 2022, Joseph Kehoe
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2022 by Joseph Kehoe <joe@pop-os>
%%%-------------------------------------------------------------------
-module(update).

-export([fib/1,fac/1]).


fib(0)->
    1;
fib(1) ->
    1;
fib(N) ->
    fib(N-1)+fib(N-2).

fac(0)->
    1;
fac(N) ->
    N+fac(N-1).
