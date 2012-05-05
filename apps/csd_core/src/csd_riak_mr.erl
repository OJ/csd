-module(csd_riak_mr).
-author('OJ Reeves <oj@buffered.io>').

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    create/0,
    run/2,
    add_input_index/5,
    add_map_js/2,
    add_map_js/3,
    add_map_js/4,
    add_reduce_js/2,
    add_reduce_js/3,
    add_reduce_js/4,
    add_reduce_sort_js/2,
    add_reduce_sort_js/3
  ]).

%% ------------------------------------------------------------------
%% Private Record Definitions
%% ------------------------------------------------------------------

-record(mr, {
    in_ind = undefined,
    %% TODO: when the need arises add support for other inputs
    %% including {bucket, key} and {bucket, key, arg}.
    phases = []
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Create a mew map/reduce job instance.
create() ->
  #mr{}.

run(RiakPid, #mr{in_ind=Input, phases=P}) ->
  % phases are pushed in reverse, so reverse them before using them
  Phases = lists:reverse(P),
  Result = riakc_pb_socket:mapred(RiakPid, Input, Phases),
  Result.

%% @doc Creates a map/reduce Input phase for a secondary index input.
add_input_index(MR=#mr{}, Bucket, Type, Index, Value) when is_integer(Value) ->
  add_input_index(MR, Bucket, Type, Index, integer_to_list(Value));
add_input_index(MR=#mr{}, Bucket, Type, Index, Value) when is_list(Value) ->
  add_input_index(MR, Bucket, Type, Index, list_to_binary(Value));
add_input_index(MR=#mr{}, Bucket, Type, Index, Value) when is_binary(Value) ->
  MR#mr{
    in_ind = {index, Bucket, csd_riak:index(Type, Index), Value}
  }.

%% @doc Creates a map/reduce Map phase from raw JS source. This overload
%%      defaults Keep to true and Arg to none.
add_map_js(MR=#mr{}, JsSource) ->
  add_map_js(MR, JsSource, true).

%% @doc Creates a map/reduce Map phase from raw JS source. This overload
%%      defaults Arg to none.
add_map_js(MR=#mr{}, JsSource, Keep) ->
  add_map_js(MR, JsSource, Keep, none).

%% @doc Creates a map/reduce Map phase from raw JS source.
add_map_js(MR=#mr{phases=P}, JsSource, Keep, Arg) ->
  MR#mr{
    phases = [{map, {jsanon, JsSource}, Arg, Keep}|P]
  }.

%% @doc Creates a map/reduce Reduce phase from raw JS source. This overload
%%      defaults Keep to true and Arg to none.
add_reduce_js(MR=#mr{}, JsSource) ->
  add_reduce_js(MR, JsSource, true).

%% @doc Creates a map/reduce Reduce phase from raw JS source. This overload
%%      defaults Keep to true.
add_reduce_js(MR=#mr{}, JsSource, Keep) ->
  add_reduce_js(MR, JsSource, Keep, none).

%% @doc Creates a map/reduce Reduce phase from raw JS source.
add_reduce_js(MR=#mr{phases=P}, JsSource, Keep, Arg) ->
  MR#mr{
    phases = [{reduce, {jsanon, JsSource}, Arg, Keep}|P]
  }.

%% @doc Creates a map/reduce Reduce sort phase using Riak's built in sort function
%%      using the specified comparison function written in raw JS. This overload
%%      defaults Keep to true.
add_reduce_sort_js(MR=#mr{}, CompareFun) ->
  add_reduce_sort_js(MR, CompareFun, true).

%% @doc Creates a map/reduce Reduce sort phase using Riak's built in sort function
%%      using the specified comparison function written in raw JS.
add_reduce_sort_js(MR=#mr{phases=P}, CompareFun, Keep) ->
  MR#mr{
    phases = [{reduce, {jsfun, <<"Riak.reduceSort">>}, CompareFun, Keep}|P]
  }.

