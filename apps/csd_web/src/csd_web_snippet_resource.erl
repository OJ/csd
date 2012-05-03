-module(csd_web_snippet_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    init/1,
    content_types_provided/2,
    to_json/2
  ]).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, State) ->
  Types = [
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

to_json(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, SnippetKey} = dict:find(key, PathInfo),
  {ok, Snippet} = csd_snippet:fetch(list_to_binary(SnippetKey)),

  {ok, Count} = case cookie:load_auth(ReqData) of
    {ok, {UserId, _, _, _}} ->
      csd_vote:count_for_snippet(SnippetKey, UserId);
    _ ->
      csd_vote:count_for_snippet(SnippetKey)
  end,

  Json = iolist_to_binary([
      "{\"snippet\":",
      csd_snippet:to_json(Snippet),
      ",\"count\":",
      csd_vote:to_json(Count),
      "}"
    ]),

  {Json, ReqData, State}.

