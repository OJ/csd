%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_web_request_resource).

-author('OJ Reeves <oj@buffered.io>').

-export([init/1, to_html/2, resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

resource_exists(ReqData, State) ->
  {false, ReqData, State}.

previously_existed(ReqData, State) ->
  {true, ReqData, State}.

moved_temporarily(ReqData, State) ->
  {ok, Url} = twitter:request_access(),
  {{true, Url}, ReqData, State}.

to_html(ReqData, State) ->
  Content = csd_view:logon(),
  {Content, ReqData, State}.
