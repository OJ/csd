-module(csd_user).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([get_id/1, fetch/1, save/1, to_user/2, from_json/1, to_json/1]).

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(user, {
    id,
    name,
    joined
  }).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

to_user(Id, Name) ->
  #user{
    name = Name,
    id = Id,
    joined = csd_date:utc_now()
  }.

get_id(#user{id=Id}) ->
  Id.

fetch(Id) ->
  csd_db:get_user(Id).

save(User=#user{}) ->
  csd_db:save_user(User).

to_json(#user{name=N, id=T, joined=J}) ->
  csd_json:to_json([{name, N}, {id, T}, {joined, J}], fun is_string/1).

from_json(UserJson) ->
  User = csd_json:from_json(UserJson, fun is_string/1),
  #user{
    id = proplists:get_value(User, id),
    name = proplists:get_value(User, name),
    joined = proplists:get_value(User, joined)
  }.

%% --------------------------------------------------------------------------------------
%% Internal Function Definitions
%% --------------------------------------------------------------------------------------

is_string(name) -> true;
is_string(joined) -> true;
is_string(_) -> false.
