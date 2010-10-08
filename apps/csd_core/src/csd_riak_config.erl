-module(csd_riak_config).

-export([connection_info/0]).

%% ---------------- Exported Functions

connection_info() ->
  { "127.0.0.1", 8080 }.

