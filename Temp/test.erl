-module(test).
-include("header.hrl").
-export([run/0]).

run() ->
    Alice = main:launch_node(alice),
    Bob = main:launch_node(bob),
    Charlie = main:launch_node(charlie),

    main:connect_node(alice, Alice, bob, Bob),
    main:connect_node(bob, Bob, charlie, Charlie),

    main:compute_nth_prime(Alice, 10, alice, bob).
