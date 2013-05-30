-module(csd_user).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    get_id/1,
    get_name/1,
    fetch/1,
    save/1,
    to_user/2,
    from_json/1,
    to_json/1]).

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
    name = csd_util:to_binary(Name),
    id = Id,
    joined = csd_util:utc_now()
  }.

get_id(#user{id=Id}) ->
  Id.

get_name(#user{name=Name}) ->
  Name.

fetch(Id) ->
  csd_db:get_user(Id).

save(User=#user{}) ->
  csd_db:save_user(User).

to_json(#user{name=N, id=T, joined=J}) ->
  jiffy:encode({[
      {<<"name">>, N},
      {<<"id">>, T},
      {<<"joined">>, J}
    ]}).

from_json(UserJson) ->
  {User} = jiffy:decode(UserJson),
  #user{
    id = proplists:get_value(<<"id">>, User),
    name = proplists:get_value(<<"name">>, User),
    joined = proplists:get_value(<<"joined">>, User)
  }.

%% --------------------------------------------------------------------------------------
%% Internal Function Definitions
%% --------------------------------------------------------------------------------------

