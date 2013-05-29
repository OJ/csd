-module(csd_vote_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"vote">>).
-define(SNIPPET_INDEX, <<"snippetid">>).
-define(WHICH_INDEX, <<"which">>).
-define(USER_INDEX, <<"userid">>).
-define(MR_MOD, csd_riak_mapreduce).
-define(MR_MAP_COUNT, map_count_votes).
-define(MR_RED_COUNT, reduce_count_votes).

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
  case count_for_snippet(RiakPid, SnippetId, <<"">>) of
    {ok, {Left, Right, _}} -> {ok, {Left, Right}};
    Error -> Error
  end.

count_for_snippet(RiakPid, SnippetId, UserId) ->
  MR1 = csd_riak_mr:add_input_index(csd_riak_mr:create(), ?BUCKET, bin,
    ?SNIPPET_INDEX, SnippetId),
  MR2 = csd_riak_mr:add_map_erl(MR1, ?MR_MOD, ?MR_MAP_COUNT, false, UserId),
  MR3 = csd_riak_mr:add_reduce_erl(MR2, ?MR_MOD, ?MR_RED_COUNT),
  case csd_riak_mr:run(RiakPid, MR3) of
    {ok, []} -> {ok, {0, 0, <<"">>}};
    {ok, [{1, [Left, Right, Which]}]} -> {ok, {Left, Right, Which}};
    Error -> Error
  end.

save(RiakPid, Vote) ->
  VoteId = csd_vote:get_id(Vote),
  UserId = csd_vote:get_user_id(Vote),
  SnippetId = csd_vote:get_snippet_id(Vote),
  Which = csd_vote:get_which(Vote),

  case csd_riak:fetch(RiakPid, ?BUCKET, VoteId) of
    {ok, _RiakObj} ->
      {error, "User has already voted on this snippet."};
    {error, notfound} ->
      RiakObj = csd_riak:create(?BUCKET, VoteId, csd_vote:to_json(Vote)),
      Indexes = [
        {bin, ?SNIPPET_INDEX, SnippetId},
        {int, ?USER_INDEX, UserId},
        {bin, ?WHICH_INDEX, Which}
      ],

      NewRiakObj = csd_riak:set_indexes(RiakObj, Indexes),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      {ok, Vote}
  end.

