-module(csd_vote_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"vote">>).
-define(SNIPPET_INDEX, <<"snippetid">>).
-define(USER_INDEX, <<"userid">>).
-define(COUNT_VOTE_MAP_JS, <<"function(v){var d=Riak.mapValuesJson(v)[0];if(d.which===\"left\"){return[[1,0]];}return[[0,1]];}">>).
-define(COUNT_VOTE_RED_JS, <<"function(vals,arg){if(vals.length===0){return[[0,0]];}return[vals.reduce(function(a,v){return[a[0]+v[0],a[1]+v[1]];})];}">>).
-define(COUNT_VOTE_USER_MAP_JS, <<"function(v,k,a){var d=Riak.mapValuesJson(v)[0];var which=d.user_id===a?d.which:\"\";if(d.which===\"left\"){return[[1,0,which]];}return[[0,1,which]];}">>).
-define(COUNT_VOTE_USER_RED_JS, <<"function(vals,arg){if(vals.length===0){return[[0,0,\"\"]];}return[vals.reduce(function(a,v){return[a[0]+v[0],a[1]+v[1],a[2].length>0?a[2]:v[2]];})];}">>).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([fetch/2, save/2, count_for_snippet/2, count_for_snippet/3]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

fetch(RiakPid, VoteId) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, VoteId) of
    {ok, RiakObj} ->
      VoteJson = csd_riak:get_value(RiakObj),
      Vote = csd_vote:from_json(VoteJson),
      {ok, Vote};
    {error, Reason} ->
      {error, Reason}
  end.

count_for_snippet(RiakPid, SnippetId) ->
  MR1 = csd_riak_mr:add_input_index(csd_riak_mr:create(), ?BUCKET, bin,
    ?SNIPPET_INDEX, SnippetId),
  MR2 = csd_riak_mr:add_map_js(MR1, ?COUNT_VOTE_MAP_JS, false),
  MR3 = csd_riak_mr:add_reduce_js(MR2, ?COUNT_VOTE_RED_JS),
  case csd_riak_mr:run(RiakPid, MR3) of
    {ok, [{1, [[Left, Right]]}]} -> {ok, {Left, Right}};
    Error -> Error
  end.

count_for_snippet(RiakPid, SnippetId, UserId) ->
  MR1 = csd_riak_mr:add_input_index(csd_riak_mr:create(), ?BUCKET, bin,
    ?SNIPPET_INDEX, SnippetId),
  MR2 = csd_riak_mr:add_map_js(MR1, ?COUNT_VOTE_USER_MAP_JS, false, UserId),
  MR3 = csd_riak_mr:add_reduce_js(MR2, ?COUNT_VOTE_USER_RED_JS),
  case csd_riak_mr:run(RiakPid, MR3) of
    {ok, [{1, [[Left, Right, Which]]}]} -> {ok, {Left, Right, Which}};
    Error -> Error
  end.

save(RiakPid, Vote) ->
  VoteId = csd_vote:get_id(Vote),
  UserId = csd_vote:get_user_id(Vote),
  SnippetId = csd_vote:get_snippet_id(Vote),

  case csd_riak:fetch(RiakPid, ?BUCKET, VoteId) of
    {ok, _RiakObj} ->
      {error, "User has already voted on this snippet."};
    {error, notfound} ->
      RiakObj = csd_riak:create(?BUCKET, VoteId, csd_vote:to_json(Vote)),
      Indexes = [
        {bin, ?SNIPPET_INDEX, SnippetId},
        {int, ?USER_INDEX, UserId}
      ],

      NewRiakObj = csd_riak:set_indexes(RiakObj, Indexes),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      {ok, Vote}
  end.

