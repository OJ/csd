%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the csd application.

-module(csd_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for csd.
start(_Type, _StartArgs) ->
    csd_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for csd.
stop(_State) ->
    ok.
