%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves

%% @doc Riak configuration functions

-module(csd_riak_config).

-export([connection_info/0]).

%% ----------------------------------------------- Exported Functions

%% @spec connection_info() -> { string, int }
%% @doc returns the IP and Port number of the riak cluster.
connection_info() ->
  { "127.0.0.1", 8080 }.

