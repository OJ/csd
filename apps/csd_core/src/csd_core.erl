-module(csd_core).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
  start_common(),
  pooler_sup:start_link().

%% @spec start() -> ok
%% @doc Start the csd_core server.
start() ->
  start_common().

%% @spec stop() -> ok
%% @doc Stop the csd_core server.
stop() ->
  application:stop(pooler),
  application:stop(flake),
  application:stop(crypto),
  ok.

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

start_common() ->
  ensure_started(crypto),
  ensure_started(flake),
  ensure_started(pooler).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

