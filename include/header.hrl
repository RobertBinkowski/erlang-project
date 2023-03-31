% If Not defined define  Number of max hoops
-define(MAX_HOPS, 15).

-record(node, {
    % Nickname on the node
    nickname,
    % Nodes Pid
    pid,
    % Nodes routing table
    routing_table
}).

-record(packet, {
    % Type of packet (e.g., request, answer)
    type,
    % Nickname of the node that sent the packet
    sender_nickname,
    % Nickname of the destination node
    destination_nickname,
    % Content of the packet
    payload,
    % Number of hops the packet has taken
    hops
}).
