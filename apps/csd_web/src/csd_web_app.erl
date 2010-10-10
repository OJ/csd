%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves

%% @doc Callbacks for the csd_web application.

-module(csd_web_app).
-author('OJ Reeves <oj@buffered.io>').

-behaviour(application).
-export([start/2, stop/1]).

%% ----------------------------------------------- Exported Functions

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for csd_web.
start(_Type, _StartArgs) ->
  csd_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for csd_web.
stop(_State) ->
  ok.
