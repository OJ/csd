%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2011 OJ Reeves

%% @doc csd_core startup code

-module(csd_core).
-author('OJ Reeves <oj@buffered.io>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_common(),
    pooler_sup:start_link().

%% @spec start() -> ok
%% @doc Start the csd_core server.
start() ->
    start_common(),
    ensure_started(pooler).

%% @spec stop() -> ok
%% @doc Stop the csd_core server.
stop() ->
    application:stop(pooler),
    application:stop(crypto),
    ok.

%% @private
start_common() ->
    ensure_started(crypto).
