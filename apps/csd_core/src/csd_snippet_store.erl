-module(csd_snippet_store).
-author('OJ Reeves <oj@buffered.io>').

-export([save/2, fetch/2, list_for_user/2]).

-define(BUCKET, <<"snippet">>).
-define(USERID_INDEX, "userid").
-define(LIST_MAP_JS, <<"function(v){var d = Riak.mapValuesJson(v)[0]; return [{key:d.key,title:d.title,created:d.created}];}">>).
-define(REDUCE_SORT_JS, <<"function(a,b){return a.created<b.created?1:(a.created>b.created?-1:0);}">>).

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

list_for_user(RiakPid, UserId) ->
  Index = csd_riak:get_mapred_phase_input_index(?BUCKET, int, ?USERID_INDEX, UserId),
  Map = csd_riak:get_mapred_phase_map_js(?LIST_MAP_JS, false),
  Sort = csd_riak:get_mapred_reduce_sort_js(?REDUCE_SORT_JS),
  %Sort = csd_riak:get_mapred_phase_reduce_js(?REDUCE_SORT_JS),
  Result = case csd_riak:mapred(RiakPid, Index, [Map, Sort]) of
    {ok, [{1, List}]} -> List;
    {ok, []} -> []
  end,
  {ok, Result}.

persist(RiakPid, RiakObj, Snippet) ->
  UserId = csd_snippet:get_user_id(Snippet),
  UpdatedRiakObj = csd_riak:set_index(RiakObj, int, ?USERID_INDEX, UserId),
  ok = csd_riak:save(RiakPid, UpdatedRiakObj),
  {ok, Snippet}.
