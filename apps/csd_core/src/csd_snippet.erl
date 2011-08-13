-module(csd_snippet).
-export([to_snippet/3, to_json/1, from_json/1, save/2, fetch/2]).

-define(BUCKET, <<"snippet">>).

to_snippet(Title, Left, Right) ->
  {snippet,
    [
      {title, Title},
      {left, Left},
      {right, Right}
    ]
  }.

to_json({snippet, SnippetData}) ->
  to_json_internal(SnippetData).

from_json(SnippetJson) ->
  from_json_internal(SnippetJson).

fetch(RiakPid, Key) ->
  case csd_riak:fetch(RiakPid, ?BUCKET, Key) of
    {ok, RiakObj} ->
      SnippetJson = csd_riak:get_value(RiakObj),
      {ok, from_json_internal(SnippetJson)};
    {error, Reason} ->
      {error, Reason}
  end.

save(RiakPid, Snippet={snippet, SnippetData}) ->
  case proplists:get_value(key, SnippetData, undefined) of
    undefined ->
      Key = csd_riak:new_key(),
      NewSnippetData = [{key, Key} | SnippetData],
      RiakObj = csd_riak:create(?BUCKET, Key, to_json_internal(NewSnippetData)),
      ok = csd_riak:save(RiakPid, RiakObj),
      {snippet, NewSnippetData};
    ExistingKey ->
      case csd_riak:fetch(RiakPid, ?BUCKET, ExistingKey) of
        {ok, RiakObj} ->
          NewRiakObj = csd_riak:update(RiakObj, to_json_internal(SnippetData)),
          ok = csd_riak:save(RiakPid, NewRiakObj),
          Snippet;
        {error, notfound} ->
          RiakObj = csd_riak:create(?BUCKET, ExistingKey, to_json_internal(SnippetData)),
          ok = csd_riak:save(RiakPid, RiakObj),
          {snippet, SnippetData}
      end
  end.

to_json_internal(SnippetData) ->
  csd_json:to_json(SnippetData, fun is_string/1).

from_json_internal(SnippetJson) ->
  {snippet, csd_json:from_json(SnippetJson, fun is_string/1)}.

is_string(title) -> true;
is_string(left) -> true;
is_string(right) -> true;
is_string(_) -> false.
