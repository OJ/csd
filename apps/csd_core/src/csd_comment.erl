-module(csd_comment).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    to_comment/4,
    list_all/1,
    list_since/2,
    save/1,
    to_json/1,
    from_json/1
  ]).

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(comment, {
    user_id,
    snippet_id,
    body,
    reply_to_id,
    time
  }).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

to_comment(UserId, SnippetId, Body, ReplyToId) ->
  #comment{
    user_id = UserId,
    snippet_id = csd_util:to_binary(SnippetId),
    body = csd_util:to_binary(Body),
    reply_to_id = csd_util:to_binary(ReplyToId),
    time = csd_date:now()
  }.

to_json(#comment{user_id=U, snippet_id=S, body=B, time=T}) ->
  jiffy:encode({[
      {<<"user_id">>, U},
      {<<"snippet_id">>, S},
      {<<"body">>, B},
      {<<"time">>, T}
    ]}).

list_all(SnippetId) ->
  csd_db:list_all_comments(SnippetId).

list_since(SnippetId, Since) ->
  csd_db:list_comments_since(SnippetId, Since).

save(Comment=#comment{}) ->
  csd_db:save_comment(Comment).

from_json(Json) ->
  {List} = jiffy:decode(Json),
  #comment{
    user_id = proplists:get_value(<<"user_id">>, List),
    snippet_id = proplists:get_value(<<"snippet_id">>, List),
    time = proplists:get_value(<<"time">>, List),
    reply_to_id = proplists:get_value(<<"reply_to_id">>, List),
    body = proplists:get_value(<<"body">>, List)
  }.

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

