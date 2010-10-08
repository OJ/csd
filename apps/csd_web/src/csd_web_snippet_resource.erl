%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(csd_web_snippet_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, SnippetKey} = dict:find(key, PathInfo),
  {snippet, SnippetData} = csd_core_svr:get_snippet(list_to_binary(SnippetKey)),
  {ok, Content} = snippet_dtl:render(SnippetData),
  {Content, ReqData, State}.
