-module(csd_web).
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
  csd_web_sup:start_link().

%% @spec start() -> ok
%% @doc Start the csd_web server.
start() ->
  start_common(),
  application:start(csd_web).

%% @spec stop() -> ok
%% @doc Stop the csd_web server.
stop() ->
  Res = application:stop(csd_web),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(public_key),
  application:stop(ssl),
  application:stop(crypto),
  application:stop(inets),
  Res.

%% --------------------------------------------------------------------------------------
%% Privae Function Definitions
%% --------------------------------------------------------------------------------------

start_common() ->
  ensure_started(inets),
  ensure_started(crypto),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(mochiweb),
  application:set_env(webmachine, webmachine_logger_module, webmachine_log),
  ensure_started(webmachine),
  ok.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

