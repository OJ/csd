-module(csd_riak).
-author('OJ Reeves <oj@buffered.io>').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([connect/1,
    create/3,
    create/4,
    fetch/3,
    update/2,
    get_value/1,
    save/2,
    get_index/3,
    set_index/4,
    set_indexes/2,
    mapred/3,
    get_mapred_phase_input_index/4,
    get_mapred_phase_map_js/1,
    get_mapred_phase_map_js/2,
    get_mapred_phase_map_js/3,
    get_mapred_phase_reduce_js/1,
    get_mapred_phase_reduce_js/2,
    get_mapred_phase_reduce_js/3,
    get_mapred_reduce_sort_js/1,
    get_mapred_reduce_sort_js/2,
    new_key/0,
    new_key/1
  ]).

-define(INDEX_KEY, <<"index">>).
-define(INDEX_SUFFIX_INT, "_int").
-define(INDEX_SUFFIX_BIN, "_bin").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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

%% @spec set_index_int(riakc_obj(), string, int) -> riakc_obj()
%% @doc Adds an integer index of the given name to the object's
%%      metadata and returns the updated object.
set_index(RiakObj, Type, Name, Value) ->
  Meta = riakc_obj:get_update_metadata(RiakObj),
  Index = case dict:find(?INDEX_KEY, Meta) of
    error -> [];
    {ok, I} -> I
  end,
  NewIndex = dict:to_list(dict:store(index(Type, Name), value(Value), dict:from_list(Index))),
  riakc_obj:update_metadata(RiakObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

set_indexes(RiakObj, Indexes) ->
  Meta = riakc_obj:get_update_metadata(RiakObj),
  Index = case dict:find(?INDEX_KEY, Meta) of
    error -> [];
    {ok, I} -> I
  end,
  UpdatedIndexes = lists:foldl(fun({T, N, V}, I) ->
        dict:store(index(T, N), value(V), I)
    end,
    dict:from_list(Index), Indexes),
  NewIndex = dict:to_list(UpdatedIndexes),
  riakc_obj:update_metadata(RiakObj, dict:store(?INDEX_KEY, NewIndex, Meta)).

%% @spec get_index_int(riakc_obj(), string) -> int
%% @doc Queries the object meta data to pull out an index of
%%      integer type. Assumes that the index exists, expect
%%      failure when querying when metadata/index missing.
get_index(RiakObj, Type, Name) ->
  Meta = riakc_obj:get_metadata(RiakObj),
  Indexes = dict:fetch(?INDEX_KEY, Meta),
  proplists:get_value(index(Type, Name), Indexes).

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

%% @doc Executes a map/reduce of the given Phases job over the Inputs.
mapred(RiakPid, Input, Phases) when is_list(Phases) ->
  Result = riakc_pb_socket:mapred(RiakPid, Input, Phases),
  Result.

%% @doc Creates a map/reduce Input phase for a secondary index input.
get_mapred_phase_input_index(Bucket, Type, Index, Value) when is_integer(Value) ->
  get_mapred_phase_input_index(Bucket, Type, Index, integer_to_list(Value));
get_mapred_phase_input_index(Bucket, Type, Index, Value) when is_list(Value) ->
  get_mapred_phase_input_index(Bucket, Type, Index, list_to_binary(Value));
get_mapred_phase_input_index(Bucket, Type, Index, Value) when is_binary(Value) ->
  {index, Bucket, index(Type, Index), Value}.

%% @doc Creates a map/reduce Map phase from raw JS source. This overload
%%      defaults Keep to true and Arg to none.
get_mapred_phase_map_js(JsSource) ->
  get_mapred_phase_map_js(JsSource, true).

%% @doc Creates a map/reduce Map phase from raw JS source. This overload
%%      defaults Arg to none.
get_mapred_phase_map_js(JsSource, Keep) ->
  get_mapred_phase_map_js(JsSource, Keep, none).

%% @doc Creates a map/reduce Map phase from raw JS source.
get_mapred_phase_map_js(JsSource, Keep, Arg) ->
  {map, {jsanon, JsSource}, Arg, Keep}.

%% @doc Creates a map/reduce Reduce phase from raw JS source. This overload
%%      defaults Keep to true and Arg to none.
get_mapred_phase_reduce_js(JsSource) ->
  get_mapred_phase_reduce_js(JsSource, true).

%% @doc Creates a map/reduce Reduce phase from raw JS source. This overload
%%      defaults Keep to true.
get_mapred_phase_reduce_js(JsSource, Keep) ->
  get_mapred_phase_reduce_js(JsSource, Keep, none).

%% @doc Creates a map/reduce Reduce phase from raw JS source.
get_mapred_phase_reduce_js(JsSource, Keep, Arg) ->
  {reduce, {jsanon, JsSource}, Arg, Keep}.

%% @doc Creates a map/reduce Reduce sort phase using Riak's built in sort function
%%      using the specified comparison function written in raw JS. This overload
%%      defaults Keep to true.
get_mapred_reduce_sort_js(CompareFun) ->
  get_mapred_reduce_sort_js(CompareFun, true).

%% @doc Creates a map/reduce Reduce sort phase using Riak's built in sort function
%%      using the specified comparison function written in raw JS.
get_mapred_reduce_sort_js(CompareFun, Keep) ->
  {reduce, {jsfun, <<"Riak.reduceSort">>}, CompareFun, Keep}.

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

%% ------------------------------------------------------------------
%% Private Function Definitions
%% ------------------------------------------------------------------

index(int, Name) ->
  iolist_to_binary([Name, ?INDEX_SUFFIX_INT]);
index(bin, Name) ->
  iolist_to_binary([Name, ?INDEX_SUFFIX_BIN]).

value(V) when is_list(V) ->
  list_to_binary(V);
value(V) ->
  V.

