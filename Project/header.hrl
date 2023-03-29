%% Header file for the prime and routing modules

-define(MAX_HOPS, 15).

-record(node, {
    nickname :: atom(),
    neighbors :: [{atom(), pid()}],
    routing_table :: [{atom(), {atom(), pid(), non_neg_integer()}}]
}).

%% is_prime/1
%% @doc Checks if a given number is prime
% -spec is_prime(integer()) -> boolean().

% %% nth_prime/1
% %% @doc Returns the nth prime number
% -spec nth_prime(pos_integer()) -> pos_integer().

% %% next_prime/1
% %% @doc Returns the smallest prime number larger than X
% -spec next_prime(pos_integer()) -> pos_integer().

% %% route/2
% %% @doc Routes the given message according to the routing table
% -spec route(node(), tuple()) -> ok.

% %% update_routing_table/3
% %% @doc Updates the routing table for the given destination and neighbor nicknames
% -spec update_routing_table(node(), atom(), atom()) -> node().

% %% get_route_pid/2
% %% @doc Given a nickname, retrieves the PID associated with it from the routing table
% -spec get_route_pid(node(), atom()) -> {atom(), pid()} | false.

% %% update_neighbor_routing_table/5
% %% @doc Updates the routing table for neighbor nodes with the given destination and neighbor nicknames and PIDs
% -spec update_neighbor_routing_table(node(), atom(), atom(), pid(), non_neg_integer()) ->
%     ok | node().
