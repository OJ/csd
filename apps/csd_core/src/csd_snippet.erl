-module(csd_snippet).
-author('OJ Reeves <oj@buffered.io>').

-export([to_snippet/3,
    to_snippet/4,
    to_json/1,
    from_json/1,
    list_for_user/1,
    fetch/1,
    save/1,
    get_data/1,
    set_user_id/2,
    get_user_id/1,
    get_key/1,
    set_key/2]).

-define(BUCKET, <<"snippet">>).

-record(snippet, {
    user_id,
    key,
    data
  }).

to_snippet(Title, Left, Right) ->
  to_snippet(Title, Left, Right, undefined).

to_snippet(Title, Left, Right, UserId) ->
  Key = csd_riak:new_key(),
  #snippet{
    data = [
      {key, Key},
      {title, Title},
      {left, Left},
      {right, Right},
      {created, csd_date:utc_now()}
    ],
    user_id = UserId,
    key = Key
  }.

list_for_user(UserId) ->
  csd_core_server:list_snippets(UserId).

fetch(SnippetKey) when is_list(SnippetKey) ->
  fetch(list_to_binary(SnippetKey));
fetch(SnippetKey) when is_binary(SnippetKey) ->
  csd_core_server:get_snippet(SnippetKey).

save(Snippet=#snippet{}) ->
  csd_core_server:save_snippet(Snippet).

get_data(#snippet{data=Data}) ->
  Data.

set_user_id(Snippet=#snippet{}, UserId) ->
  Snippet#snippet{
    user_id = UserId
  }.

get_user_id(#snippet{user_id=UserId}) ->
  UserId.

get_key(#snippet{key=Key}) ->
  Key.

set_key(Snippet=#snippet{data=Data}, NewKey) ->
  Snippet#snippet{
    key = NewKey,
    data = dict:to_list(dict:store(key, NewKey, dict:from_list(Data)))
  }.

to_json(#snippet{data=Data}) ->
  csd_json:to_json(Data, fun is_string/1).

from_json(SnippetJson) ->
  Data = csd_json:from_json(SnippetJson, fun is_string/1),
  #snippet{
    data = Data
  }.

is_string(title) -> true;
is_string(left) -> true;
is_string(right) -> true;
is_string(created) -> true;
is_string(_) -> false.
