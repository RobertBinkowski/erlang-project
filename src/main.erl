-module(main).
-export([start/0]).

% -include("header.hrl").

%% @doc Start The application
start() ->
    %% Start the node process
    node:start(),

    %% Start the routing process
    routing:start(),

    %% Run tests
    test:start().
