-module(csd_user_store).
-author('OJ Reeves <oj@buffered.io>').

-export([fetch/2, save/2]).

-define(BUCKET, <<"user">>).

fetch(RiakPid, UserId) when is_integer(UserId) ->
  fetch(RiakPid, integer_to_list(UserId));
fetch(RiakPid, UserId) when is_list(UserId) ->
  fetch(RiakPid, list_to_binary(UserId));
fetch(RiakPid, UserId) when is_binary(UserId) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, UserId) of
    {ok, RiakObj} ->
      UserJson = csd_riak:get_value(RiakObj),
      User = csd_user:from_json(UserJson),
      {ok, User};
    {error, Reason} ->
      {error, Reason}
  end.

save(RiakPid, User) ->
  IntId = csd_user:get_id(User),

  % Id is int, so we need to conver to a binary
  UserId = list_to_binary(integer_to_list(IntId)),

  case csd_riak:fetch(RiakPid, ?BUCKET, UserId) of
    {ok, _RiakObj} ->
      % user already exists, we don't need to save anything.
      {ok, User};
    {error, notfound} ->
      NewRiakObj = csd_riak:create(?BUCKET, UserId, csd_user:to_json(User)),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      {ok, User}
  end.
