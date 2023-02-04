%%%-------------------------------------------------------------------
%%% @author Joseph <joseph@josephdesktop>
%%% @copyright (C) 2021, Joseph
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2021 by Joseph <joseph@josephdesktop>
%%%-------------------------------------------------------------------
-module(concurrency).
-export([reverse/1,reverseServer/0,revServer/1,replyServer/1,rpc/2,maxp/1,on_exit/2,keep_alive/2,sleep/1,flush_mailbox/0,priority_receive/0,smallfun/0]).

%Basic Reverse function
reverse(List)->
    reverseAcc(List,[]).
reverseAcc([],SoFar)->
    SoFar;
reverseAcc([Head|Tail],SoFar) ->
    reverseAcc(Tail,[Head|SoFar]).

%Reverse running as a process
%Pid=spawn(concurrency,reverseServer,[]).
reverseServer()->
    receive
        List ->
            Answer=reverse(List),
            io:format("Reversed list is:~p~n",[Answer])
    end,
    reverseServer().

%reverse server with a state

revServer(N)->
    receive
        List->
            Answer=reverse(List),
            io:format("List is:~p  Called ~p times~n",[Answer,N+1]),
            revServer(N+1)
end.

%server that replies to sender
replyServer(N)->
    receive
        {Sender,List}->
            Answer=reverse(List),
            io:format("List is:~p  Called ~p times~n",[Answer,N+1]),
            Sender!{self(),Answer},
            replyServer(N+1)
    end.    

%Add RPC functionality

rpc(Pid,Message)->
    Pid!{self(),Message},
    receive
        {Pid,Reply}->
            Reply
end.

maxp(N)->
    Max=erlang:system_info(process_limit),
    io:format("Maximum allowed processes:~p~n",[Max]),
    statistics(runtime),
    statistics(wall_clock),
    L=for(1,N,fun()->spawn(fun()->wait()end)end),
    {_,Time1}=statistics(runtime),
    {_,Time2}=statistics(wall_clock),
    lists:foreach(fun(Pid)->Pid!die end,L),
    U1=Time1*1000/N,
    U2=Time2*1000/N,
    io:format("Process spawn time=~p (~p) microseconds~n",[U1,U2]).

wait()->
    receive
        die-> void
    end.
for(N,N,F)->
    [F()];
for(I,N,F) ->
    [F()|for(I+1,N,F)].

    

%sleep using timeout

sleep(T)->
    receive
        after T ->
                true
        end.

flush_mailbox()->
    receive _->
            flush_mailbox()
        after 0  ->
            true
        end.

priority_receive()->
    receive
        {nigel,X}->
            {nigel,X}
    after 0 ->
        receive
            Any->
                Any
        end
    end.



smallfun()->
    receive
    X->
            list_to_atom(X),
            io:format("my pid is:~p",[self()])
    end,
    smallfun().

 %Fault tolerant Server
 %Here we set up  our server to tell us when it fails
on_exit(PID,FUN)->
    spawn(fun()-> Ref=monitor(process,PID),
                  receive
                      {'DOWN',Ref,process,PID,Reason}->FUN(Reason)
                  end
          end).
%spawn a smallfun instance
%Pid=spawn(concurrency,smallfun,[]).
% attach instance to function using on_exit
%concurrency:on_exit(Pid,fun(Reason)->io:format("~p died because ~p",[Pid,Reason]) end).

%This server cannot die 
% concurrency:keep_alive(fred,fun()->concurrency:smallfun() end).

keep_alive(Name,Fun)->
    Pid=spawn(Fun),
    register(Name,Pid),
    on_exit(Pid, fun(_Reason)->keep_alive(Name,Fun) end).


