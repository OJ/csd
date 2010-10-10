%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves
%% @doc Example webmachine_resource.

-module(csd_web_resource).
-author('OJ Reeves <oj@buffered.io>').

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ----------------------------------------------- Exported Functions

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  {ok, Content} = sample_dtl:render([{param, "Slartibartfast"}]),
  {Content, ReqData, State}.
