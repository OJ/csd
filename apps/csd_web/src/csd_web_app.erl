%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the csd_web application.

-module(csd_web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for csd_web.
start(_Type, _StartArgs) ->
    csd_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for csd_web.
stop(_State) ->
    ok.
