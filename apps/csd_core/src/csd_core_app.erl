%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves

%% @doc csd_core application code.

-module(csd_core_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ----------------------------------------------- Exported Functions

start(_StartType, _StartArgs) ->
    csd_core_sup:start_link().

stop(_State) ->
    ok.
