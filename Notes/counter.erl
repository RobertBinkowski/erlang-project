%%%-------------------------------------------------------------------
%%% @author Joseph Kehoe <joe@pop-os>
%%% @copyright (C) 2022, Joseph Kehoe
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2022 by Joseph Kehoe <joe@pop-os>
%%%-------------------------------------------------------------------
-module(counter).
-export([facForEver/1,printCountdown/1]).

facForEver(N)->
    F=update:fac(N),
    io:format("fac is: ~p~n",[F]),
    timer:sleep(1000),
    facForEver(N).


printCountdown(N) when N=:=0->
    io:format("BLAST OFF~n");

printCountdown(N)->
    io:format("T minus ~p~n",[N]),
    timer:sleep(1000),
    printCountdown(N-1).
