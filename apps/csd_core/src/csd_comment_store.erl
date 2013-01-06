-module(csd_comment_store).
-author('OJ Reeves <oj@buffered.io>').

-define(BUCKET, <<"comment">>).
-define(SNIPPET_INDEX, <<"snippetid">>).
-define(USER_INDEX, <<"userid">>).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([fetch/2, save/3, list_all/2, list_all_since/3]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

fetch(RiakPid, CommentId) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, CommentId) of
    {ok, RiakObj} ->
      CommentJson = csd_riak:get_value(RiakObj),
      Comment = csd_comment:from_json(CommentJson),
      {ok, Comment};
    {error, Reason} ->
      {error, Reason}
  end.

save(RiakPid, Comment, CurrentUserId) ->
  CommentId = csd_comment:get_id(Comment),
  UserId = csd_comment:get_user_id(Comment),
  SnippetId = csd_comment:get_snippet_id(Comment),

  case csd_riak:fetch(RiakPid, ?BUCKET, CommentId) of
    {ok, RiakObj} ->
      case csd_riak:get_index(RiakObj, int, ?USER_INDEX) of
        CurrentUserId ->
          NewRiakObj = csd_riak:update(RiakObj, csd_comment:to_json(Comment)),
          ok = csd_riak:save(RiakPid, NewRiakObj),
          {ok, Comment};
        _ ->
          {error, "User can only modify their own comments."}
      end;
    {error, notfound} ->
      RiakObj = csd_riak:create(?BUCKET, CommentId, csd_comment:to_json(Comment)),
      Indexes = [
        {bin, ?SNIPPET_INDEX, SnippetId},
        {int, ?USER_INDEX, UserId}
      ],

      NewRiakObj = csd_riak:set_indexes(RiakObj, Indexes),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      {ok, Comment}
  end.


list_all(_RiakPid, _SnippetId) ->
  ok.

list_all_since(_RiakPid, _SnippetId, _Since) ->
  ok.

