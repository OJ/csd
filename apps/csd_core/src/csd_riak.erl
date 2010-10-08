-module(csd_riak).

-export([connect/1, create/3, create/4, fetch/3, update/2, get_value/1, save/2]).
-export([new_key/0, new_key/1]).

%% ---------------- Exported Functions

connect({IP, Port}) ->
  {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
  RiakPid.

create(Bucket, Key, JsonData) ->
  create(Bucket, Key, JsonData, "application/json").

create(Bucket, Key, Item, MimeType) ->
  RiakObj = riakc_obj:new(Bucket, Key, Item, MimeType),
  RiakObj.

fetch(RiakPid, Bucket, Key) ->
  RiakObj = riakc_pb_socket:get(RiakPid, Bucket, Key),
  RiakObj.

update(RiakObj, NewValue) ->
  NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
  NewRiakObj.

get_value(RiakObj) ->
  Value = riakc_obj:get_value(RiakObj),
  Value.

save(RiakPid, RiakObj) ->
  riakc_pb_socket:put(RiakPid, RiakObj).

new_key() ->
  {{Yr, Mo, Dy}, {Hr, Mn, Sc}} = erlang:universaltime(),
  {_, _, Now} = now(),
  new_key([Yr, Mo, Dy, Hr, Mn, Sc, node(), Now]).

new_key(List) ->
  Hash = erlang:phash2(List),
  base64:encode(<<Hash:32>>).

