-module(csd_web_app).
-author('OJ Reeves <oj@buffered.io>').

-behaviour(application).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([start/2, stop/1]).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for csd_web.
start(_Type, _StartArgs) ->
  csd_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for csd_web.
stop(_State) ->
  ok.

