%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves

%% @doc Functions which wrap up the communication with the Riak cluster
%%      plus a few other helper functions.

-module(csd_riak).
-author('OJ Reeves <oj@buffered.io>').

%% Riak exports
-export([connect/1, create/3, create/4, fetch/3, update/2, get_value/1, save/2]).

%% helper functions for generating unique keys.
-export([new_key/0, new_key/1]).

%% ----------------------------------------------- Exported Functions

%% @spec connect(connection_info()) -> pid()
%% @doc Create a connection to the specified Riak cluster and
%%      return the Pid associated with the new connection.
connect({IP, Port}) ->
  {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
  RiakPid.

%% @spec create(binary, binary, json) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      assumes that the data passed in is JSON and sets
%%      the MIME type to "application/json" for you.
create(Bucket, Key, JsonData) ->
  create(Bucket, Key, JsonData, "application/json").

%% @spec create(binary, binary, term(), string) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      takes arbitrary data and requires the user to
%%      specify the mime type of the data that is being
%%      stored.
create(Bucket, Key, Item, MimeType) ->
  RiakObj = riakc_obj:new(Bucket, Key, Item, MimeType),
  RiakObj.

%% @spec fetch(pid(), binary, binary) -> riakc_obj()
%% @doc Fetches a riakc object from a Riak node/cluster
%%      using the connection given.
fetch(RiakPid, Bucket, Key) ->
  RiakObj = riakc_pb_socket:get(RiakPid, Bucket, Key),
  RiakObj.

%% @spec update(riakc_obj(), term()) -> riakc_obj()
%% @doc Updates the stored value for a riakc object with
%%      the new one specified.
update(RiakObj, NewValue) ->
  NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
  NewRiakObj.

%% @spec get_value(riakc_obj()) -> term()
%% @doc Retrieves the stored value from within the riakc
%%      object.
get_value(RiakObj) ->
  Value = riakc_obj:get_value(RiakObj),
  Value.

%% @spec save(pid(), riakc_obj()) -> {ok, riakc_obj()} | {error | Reason}
%% @doc Saves the given riak object to the specified Riak node/cluster.
save(RiakPid, RiakObj) ->
  Result = riakc_pb_socket:put(RiakPid, RiakObj),
  Result.

%% @spec new_key() -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak. This implementation is blatantly borrowed
%%      (purloined) from the wriaki source (thanks basho!)
new_key() ->
  {{Yr, Mo, Dy}, {Hr, Mn, Sc}} = erlang:universaltime(),
  {_, _, Now} = now(),
  new_key([Yr, Mo, Dy, Hr, Mn, Sc, node(), Now]).

%% @spec new_key(list()) -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak using the given list parameter as the stuff
%%      to hash.
new_key(List) ->
  Hash = erlang:phash2(List),
  base64:encode(<<Hash:32>>).
