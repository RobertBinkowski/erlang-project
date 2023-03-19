%%%-------------------------------------------------------------------
%%% @author Robert Binkowski - C00237917
%%% @copyright (C) 2022, Robert Binkowski
%%%-------------------------------------------------------------------
-module(project).
-export([start/0]).
% -export([computeNthPrime/1]).
%%-include_lib("eunit/include/eunit.hrl").

start() ->
    io:format("Hello World~n").

% Figure out the nth prime
% computeNthPrime([])->
%     [];
% computeNthPrime([H|T])->
%     List = lists.filter(fun(N) -> rem H /= 0 end, T);
%     [H|computeNthPrime(List)];
% computeNthPrime(N)->
%     computeNthPrime(Lists: seq(2,N)).
