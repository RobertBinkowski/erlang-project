-module(test).
-include("header.hrl").
-export([run/0]).

run() ->
    Alice = project:launch_node(alice),
    Bob = project:launch_node(bob),
    Charlie = project:launch_node(charlie),

    project:connect_node(alice, Alice, bob, Bob),
    project:connect_node(bob, Bob, charlie, Charlie),

    project:compute_nth_prime(Alice, 10, alice, bob),
    project:display_result().
