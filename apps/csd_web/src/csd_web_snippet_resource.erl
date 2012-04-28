-module(csd_web_snippet_resource).
-author('OJ Reeves <oj@buffered.io>').

-export([init/1,
    content_types_provided/2,
    to_html/2,
    to_json/2
  ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, State) ->
  Types = [
    {"text/html", to_html},
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

to_json(ReqData, State) ->
  Snippet = get_snippet(ReqData),
  Json = csd_snippet:to_json(Snippet),
  {Json, ReqData, State}.

to_html(ReqData, State) ->
  Snippet = get_snippet(ReqData),
  SnippetData = csd_snippet:get_data(Snippet),
  {ok, Content} = snippet_dtl:render(SnippetData),
  {Content, ReqData, State}.

get_snippet(ReqData) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, SnippetKey} = dict:find(key, PathInfo),
  {ok, Snippet} = csd_snippet:fetch(list_to_binary(SnippetKey)),
  Snippet.
