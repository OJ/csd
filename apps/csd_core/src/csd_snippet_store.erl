-module(csd_snippet_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"snippet">>).
-define(USERID_INDEX, "userid").
-define(LIST_MAP_JS, <<"function(v){var d = Riak.mapValuesJson(v)[0]; return [{key:d.key,title:d.title,created:d.created}];}">>).
-define(REDUCE_SORT_JS, <<"function(a,b){return a.created<b.created?1:(a.created>b.created?-1:0);}">>).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([save/2, fetch/2, list_for_user/3, list_for_user/4]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

fetch(RiakPid, Key) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, Key) of
    {ok, RiakObj} ->
      SnippetJson = csd_riak:get_value(RiakObj),
      Snippet = csd_snippet:from_json(SnippetJson),
      UserId = csd_riak:get_index(RiakObj, int, ?USERID_INDEX),
      {ok, csd_snippet:set_user_id(Snippet, UserId)};
    {error, Reason} ->
      {error, Reason}
  end.

save(RiakPid, Snippet) ->
  Key = csd_snippet:get_key(Snippet),
  case csd_riak:fetch(RiakPid, ?BUCKET, Key) of
    {ok, RiakObj} ->
      NewRiakObj = csd_riak:update(RiakObj, csd_snippet:to_json(Snippet)),
      persist(RiakPid, NewRiakObj, Snippet);
    {error, notfound} ->
      RiakObj = csd_riak:create(?BUCKET, Key, csd_snippet:to_json(Snippet)),
      persist(RiakPid, RiakObj, Snippet)
  end.

list_for_user(RiakPid, UserId, PageSize) ->
  list_for_user(RiakPid, UserId, PageSize, 0).

list_for_user(RiakPid, UserId, PageSize, PageNumber) ->
  Fields = [<<"key">>, <<"title">>, <<"created">>],
  Opts = [{presort, <<"key">>}, {rows, PageSize}, {fl, Fields}, {start, PageSize * PageNumber}],
  Search = iolist_to_binary([<<"user_id:">>, UserId]),
  Result = csd_riak:search(RiakPid, ?BUCKET, Search, Opts),
  {ok, {search_results, Results, _, Rows}} = Result,
  Adjusted = [proplists:delete(<<"id">>, Props) || {<<"snippet">>, Props} <- Results],
  {ok, {Adjusted, Rows}}.

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

persist(RiakPid, RiakObj, Snippet) ->
  UserId = csd_snippet:get_user_id(Snippet),
  UpdatedRiakObj = csd_riak:set_index(RiakObj, int, ?USERID_INDEX, UserId),
  ok = csd_riak:save(RiakPid, UpdatedRiakObj),
  {ok, Snippet}.

