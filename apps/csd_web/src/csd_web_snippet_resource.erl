%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves
%% @doc Webmachine resource that handles snippet-related actions

-module(csd_web_snippet_resource).
-author('OJ Reeves <oj@buffered.io>').

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, SnippetKey} = dict:find(key, PathInfo),
  {snippet, SnippetData} = csd_core_server:get_snippet(list_to_binary(SnippetKey)),
  {ok, Content} = snippet_dtl:render(SnippetData),
  {Content, ReqData, State}.

