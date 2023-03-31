-define(HEADER_HRL, true).

% If Not defined define  Number of max hoops
-define(MAX_HOOPS, 15).

-record(node, {nickname, pid, routing_table}).

-record(packet, {source_nickname, destination_nickname, sender_nickname, hops}).
