%%%-------------------------------------------------------------------
%%% @author Joseph Kehoe <joe@pop-os>
%%% @copyright (C) 2023, Joseph Kehoe
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2023 by Joseph Kehoe <joe@pop-os>
%%%-------------------------------------------------------------------
-module(concurrency).
-export([fac/1,helloWorld/0,rosie/1,facServer/0,rpc/2,facAsker/2]).

fac(0)->
    1;
fac(N)when N>0 ->
    N*fac(N-1).

helloWorld()->
    io:format("Hello world~n").

rosie(State)->
    receive
      gold ->
          io:format("Not that kind of atom you fool~n"),
            rosie(State+1);
      {hello,N} ->
          io:format("you sent me: ~p~n",[N]), rosie(State+1);
      [_|Rest] ->
          io:format("the rest is:~p~n",[Rest]), rosie(State+1);
      halt ->
          io:format("halting now after ~p messages received",[State]);
      X ->
          io:format("No idea what ~p is~n",[X]),
          rosie(State+1)
    end.

facServer()->
    receive
      halt->
          io:format("DONE");
      {SenderID,N}->
          io:format("calculating...~n"),
          Reply=fac(N),
          SenderID!{self(),Reply},
            facServer()
      after 50000 ->
          io:format("I am SOOO BORED~n~n"),
            facServer()
end.

facAsker(ServerID,Value)->
    ServerID!{self(),Value},
    receive
      {ServerID,Answer}->
          io:format("factorial of ~p is ~p ~n",[Value,Answer])
    end,
    sleep(5000),
    facAsker(ServerID,Value+1).

sleep(T)->
    receive
      after T ->
            true
end.

rpc(Pid,MSG)->
    Pid!{self(),MSG},
    receive
      {Pid,Reply}->
          io:format("~nThe answer from facServer is:~p~n",[Reply])
end.
   