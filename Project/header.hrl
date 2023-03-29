%% Header file for the prime and routing modules

%% No of maximum hoops
-define(MAX_HOPS, 15).

-record(node, {
    nickname :: atom(),
    neighbors :: [{atom(), pid()}],
    routing_table :: [{atom(), {atom(), pid(), non_neg_integer()}}]
}).
