-module(node).
-export([
    start/0, launchNode/1, connectNode/4, node_loop/1, getPid/1, printTable/1, is_connected/2
]).

-include("../include/header.hrl").

start() ->
    % Create list of nodes with random names
    Nodes = lists:map(
        fun(_) ->
            % Create a random number
            RandomInt = rand:uniform(10000),
            % Generate a node name with a random number attached
            Nickname = "node" ++ integer_to_list(RandomInt),
            % Create a node
            {Nickname, launchNode(Nickname)}
        end,
        % The size of the list
        lists:seq(1, 5)
    ),
    % Get their Pid's
    Pids = [getPid(Nickname) || {Nickname, _} <- Nodes],

    % Connect each node to its subsequent node in the list
    lists:foreach(
        fun({Nickname, _}, Index) ->
            NextIndex =
                if
                    Index < length(Nodes) -> Index + 1;
                    true -> 1
                end,
            {NextNickname, _} = lists:nth(NextIndex, Nodes),
            connectNode(Nickname, getPid(Nickname), NextNickname, getPid(NextNickname))
        end,
        lists:zip(Nodes, lists:seq(1, length(Nodes)))
    ),

    io:format("Start of the Node Module with nodes: ~p~n", [Pids]).

% Spawn a node with a given name
launchNode(Nickname) ->
    % Create a new Process
    Pid = spawn(fun() ->
        % This code spawns the node_loop process
        % initialize a new Node with
        node_loop(#node{nickname = atomize(Nickname), pid = self(), routing_table = []})
    end),
    % register an atom version of Nickname with a Pid
    register(atomize(Nickname), Pid),
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

% Connect Two nodes together
connectNode(NicknameOne, PidOne, NicknameTwo, PidTwo) ->
    % Connect One node to the other
    PidOne ! {connect, PidTwo, NicknameTwo},
    PidTwo ! {connect, PidOne, NicknameOne},
    % Return True
    % Check if the nodes are connected and return the result
    node1_connected_to_node2 = is_connected(NicknameOne, NicknameTwo),
    node2_connected_to_node1 = is_connected(NicknameTwo, NicknameOne),

    % Return true if both nodes have each other in their routing tables
    node1_connected_to_node2 andalso node2_connected_to_node1.

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
