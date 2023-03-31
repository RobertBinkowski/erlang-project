-module(node).
-export([
    start/0,
    launchNode/1,
    connectNode/4,
    node_loop/1,
    getPid/1,
    printTable/1,
    is_connected/2,
    request_nth_prime/3
]).

-include("../include/header.hrl").

start() ->
    % Create list of nodes with fixed names
    NodeNames = ["nodeA", "nodeB", "nodeC", "nodeD", "nodeE"],
    Nodes = [{Nickname, launchNode(Nickname)} || Nickname <- NodeNames],

    % Get their Pid's
    Pids = [getPid(Nickname) || {Nickname, _} <- Nodes],

    % Connect each node to its subsequent node in the list
    lists:foreach(
        fun({{Nickname, _}, Index}) ->
            NextIndex =
                if
                    Index < length(Nodes) -> Index + 1;
                    true -> 1
                end,
            {NextNickname, _} = lists:nth(NextIndex, Nodes),
            _ = connectNode(Nickname, getPid(Nickname), NextNickname, getPid(NextNickname))
        end,
        lists:zip(Nodes, lists:seq(1, length(Nodes)))
    ),

    io:format("Start of the Node Module with nodes: ~p~n", [Pids]).

launchNode(Nickname) ->
    % Create a new Process
    Pid = spawn(fun() ->
        % This code spawns the node_loop process
        % initialize a new Node with
        node_loop(#node{nickname = atomize(Nickname), pid = self(), routing_table = []})
    end),
    % Check if the name is already registered
    case whereis(atomize(Nickname)) of
        undefined ->
            % If not registered, create a new process
            register(atomize(Nickname), Pid);
        OldPid ->
            % If the name is already registered, unregister and kill process
            unregister(atomize(Nickname)),
            exit(OldPid, kill),
            % Re-Register the new process
            register(atomize(Nickname), Pid)
    end,
    % Return nodePid
    Pid.

% Convert to atom if it is a string so it can be used as an identifier
atomize(Nickname) when is_atom(Nickname) ->
    Nickname;
atomize(Nickname) when is_list(Nickname) ->
    list_to_atom(Nickname).

% The main node loop
node_loop(Node) ->
    receive
        % Connect Two Nodes
        {connect, NewNeighbor, NeighborNickname} ->
            NewRoutingTable = Node#node.routing_table ++ [{NeighborNickname, NewNeighbor}],
            node_loop(Node#node{routing_table = NewRoutingTable});
        % Check if two Nodes are connected
        {is_connected, OtherNickname, Sender} ->
            Connected = lists:keymember(OtherNickname, 1, Node#node.routing_table),
            Sender ! {is_connected, Connected},
            node_loop(Node);
        % Update the routing table
        {update_routing_table, DestinationNickname, SenderNickname, Hops} ->
            NewNode = routing:handle_update_routing_table(
                Node, DestinationNickname, SenderNickname, Hops
            ),
            node_loop(NewNode);
        % Compute N'th Prime
        {compute_nth_prime, N, DestinationNickname, SenderNickname, Hops} ->
            routing:compute_nth_prime(Node, N, DestinationNickname, SenderNickname, Hops),
            node_loop(Node);
        % Receive an Answer
        {receive_answer, N, M, DestinationNickname, SenderNickname, Hops} ->
            routing:receive_answer(Node, N, M, DestinationNickname, SenderNickname, Hops),
            node_loop(Node);
        % print The routing table table
        {print_routing_table, Sender} ->
            Sender ! {routing_table, Node#node.routing_table},
            node_loop(Node);
        % Returns Node Pid
        {getPid, Sender} ->
            Sender ! {pid, Node#node.pid},
            node_loop(Node);
        % keeps the process running even if the receives do not match any cases
        _ ->
            node_loop(Node)
    end.

% Return the PID of a node with it's nickname
getPid(Nickname) ->
    % Ensure to change the Nickname to a tuple to reduce Errors
    case whereis(atomize(Nickname)) of
        % If not found return information and undefined
        undefined ->
            io:format("Node ~p not found.~n", [Nickname]),
            undefined;
        % If Found then return Pid
        Pid ->
            Pid
    end.

request_nth_prime(N, RequesterNickname, DestinationNickname) ->
    RequesterPid = getPid(RequesterNickname),
    RequesterPid ! {self(), {request_nth_prime, N, DestinationNickname, RequesterNickname}},
    receive
        {Node, {request_nth_prime, _, _, _}} ->
            routing:compute_nth_prime(Node, N, DestinationNickname, RequesterNickname, 1)
    end.

% Connect Two nodes together
connectNode(NicknameOne, PidOne, NicknameTwo, PidTwo) ->
    % Connect One node to the other
    PidOne ! {connect, PidTwo, NicknameTwo},
    PidTwo ! {connect, PidOne, NicknameOne},

    % Check if the nodes are connected and return the result
    Node1ConnectedToNode2 = is_connected(NicknameOne, NicknameTwo),
    Node2ConnectedToNode1 = is_connected(NicknameTwo, NicknameOne),

    % Return true if both nodes have each other in their routing tables
    Node1ConnectedToNode2 andalso Node2ConnectedToNode1.

% Check if two are connected
is_connected(Nickname1, Nickname2) ->
    Pid1 = getPid(Nickname1),
    Pid1 ! {is_connected, Nickname2, self()},
    receive
        {is_connected, Connected} -> Connected
    end.

% Print The PID Table with the PID
printTable(Pid) ->
    Pid ! {print_table, self()},
    receive
        {print_table, Table} ->
            io:format("Routing table: ~p~n", [Table])
    end.
