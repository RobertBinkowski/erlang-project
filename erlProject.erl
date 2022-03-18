%%%-------------------------------------------------------------------
%%% @author Robert Binkowski - C00237917
%%% @copyright (C) 2022, Robert Binkowski
%%% @doc
%%%
%%% @end
%%% Created : 17 Mar 2022 by Robert Binkowski
%%%-------------------------------------------------------------------
-module(erlProject).
-export([len/1,fastLength/1,sortServ/1,launchNode/1,printTable/1]).

% len([])->
%     0;

% len([_|Tail])->
%     1+len(Tail).

% fastLength([])->
%     0;
% fastLength([_|Tail])->


sortServ(N)->
    receive
        {Sender,List} ->
            SortedList=sort(List),
            io:format("call no. ~p: ~p~n",[N,SortedList]),
            Sender!{self(),NewList}
        end,
        sortServ(N+1).



launchNode(N)->
    end.

connectNode(NicknameOne,PidOne,NickanmeTwo,PidTwo)->
    end.

printTable(Pid)->
    end.

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
