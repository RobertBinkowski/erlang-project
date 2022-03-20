%%%-------------------------------------------------------------------
%%% @author Robert Binkowski - C00237917
%%% @copyright (C) 2022, Robert Binkowski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2022 by Robert Binkowski
%%%-------------------------------------------------------------------
-module(erlProject).
-export([
    spawn/2,
    sort/1,
    sortServ/1,
    rpc/2,
    launchNode/1,
    connectNode/4,
    printTable/1,
    on_exit/2,
    keep_alive/2,
    nth/2,
    sorty/2,
    sortServe/1
]).

% Process Creation
spawn(Module, Name, Args) -> pid()
    Module = Name = atom()
    Args = [Arg1,...,ArgN]
        ArgI = term()

% Object Creation
% define(USER_BUCKET, <<"Human">>)
% define(MSG_BUCKET, <<"Messages">>)

% record(USER_BUCKET, {user_name})
% record(MSG_BUCKET,)

nth(N,[Head,Tail])->
    nth(N-1, Tail)

% sort
sort([Head | Tail]) ->
    insert(Head, sort(Tail)).

sort([A, B]) when A > B ->
    [A, B];
sort([A, B]) ->
    [B, A];
sort([Head | Tail]) ->
    sort([X || X <- Tail, X < Head]) ++ [Head] ++
        sort([X || X <- Tail, X >= Head]).

sortServ(N) ->
    receive
        {Sender, List} ->
            SortedList = sort(List),
            io:format("call no. ~p: ~p~n", [N, SortedList]),
            Sender ! {self(), SortedList}
    end,
    sortServ(N + 1).

sorty(Pid, List) ->
    Pid ! {self(), List},
    receive
        {Pid, Answer} ->
            io:format("answer received~n")
    end.

% RPC Functionality
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {Pid, Reply} ->
            Reply
    end.

% Main Project Parts
% Node()->
%     end.

launchNode(N)->
    Nickname = N
    end.

computeNthPrime(computeNthPrime, N, DestinationNickname, SenderNickname,Hops)->
    N*computeNthPrime(N-1)


connectNode(NicknameOne,PidOne,NickanmeTwo,PidTwo)->
    
    end.

printTable(Pid)->
    end.

% Keep alive restart when killed
on_exit(PID, FUN) ->
    spawn(fun() ->
        Ref = monitor(process, PID),
        receive
            {'DOWN', Ref, process, PID, Reason} -> FUN(Reason)
        end
    end).

keep_alive(Name, FUN) ->
    Pid = spawn(FUN),
    register(Name, Pid),
    on_exit(Pid, fun(_Reason) -> keep_alive(Name, FUN) end).

% -export([member/2,delete/2,sum/1,max/1,zip/2,sort/1,rev/1,aSort/1]).

% member(_,[])->
%     false;
% member(X,[X|_]) ->
%     true;
% member(X,[_|Tail]) ->
%     member(X,Tail).

% delete(_,[])->
%     [];
% delete(X,[X|Tail]) ->
%     Tail;
% delete(X,[Head|Tail]) ->
%     [Head|delete(X,Tail)].

% sum([])->
%     0;
% sum([Head|Tail]) ->
%     Head+sum(Tail).

% max([Head|Tail])->
%     jmax(Head,Tail).

% jmax(MaxSoFar,[])->
%     MaxSoFar;
% jmax(MaxSoFar,[Head|Tail]) when MaxSoFar>Head->
%     jmax(MaxSoFar,Tail);
% jmax(_,[Head|Tail]) ->
%     jmax(Head,Tail).



% uglymax([X])->
%     X;
% uglymax([Head|Tail]) ->
%     TailMax=max(Tail),
%     if
%         Head > TailMax ->
%             Head;
%         true ->
%             TailMax
%     end.

% zip([],[])->
%     [];
% zip([Head1|Tail1],[Head2|Tail2]) ->
%     [{Head1,Head2}|zip(Tail1,Tail2)].

% reverse([])->
%     [];
% reverse([Head|Tail]) ->
%     addToBack(Head, reverse(Tail)).

% addToBack(X,[])->
%     [X];
% addToBack(X,[Head|Tail]) ->
%     [Head|addToBack(X,Tail)].

% rev(List)->
%     rev([],List).

% rev(ReversedSoFar,[])->
%     ReversedSoFar;
% rev(ReversedSoFar,[Head|Tail]) ->
%     rev([Head|ReversedSoFar],Tail).

% % rev([1,2,3]) ->
% % rev([],[1,2,3]) ->
% % rev([1],[2,3]) ->
% % rev([2,1],[3]) ->
% % rev([3,2,1],[])->
% % [3,2,1]


% sort([])->
%     [];
% sort([Head|Tail]) ->
%     insert(Head,sort(Tail)).

% insert(Head,[])->
%     [Head];
% insert(X,[Head|Tail]) when X>Head->
%     [X,Head|Tail];
% insert(X,[Head|Tail]) ->
%     [Head|insert(X,Tail)].

% last([V])->
%     V;
% last([_|Tail]) ->
%     last(Tail).

% nth(0,[Head|Tail])->
%     Head;
% nth(N,[Head|Tail]) ->
%     nth(N-1,Tail).

% aSort([])->
%     [];
% aSort([A,B]) when A>B->
%     [A,B];
% aSort([A,B]) ->
%     [B,A];
% aSort([Head|Tail]) ->
%     aSort([X||X<-Tail,X<Head])++[Head]++
%         aSort([X||X<-Tail,X>=Head]).
