-module(csd_riak_mapreduce).
-author('OJ Reeves <oj@buffered.io>').

-export([map_count_votes/3, reduce_count_votes/2]).

-define(WHICH_INDEX, <<"which_bin">>).
-define(USER_INDEX, <<"userid_int">>).

map_count_votes({error, notfound}, _KeyData, _Arg) ->
  [];
map_count_votes(RiakObject, _KeyData, CurrentUserId) ->
  Meta = riak_object:get_metadata(RiakObject),
  Indexes = dict:fetch(<<"index">>, Meta),

  Which = proplists:get_value(?WHICH_INDEX, Indexes),
  User = proplists:get_value(?USER_INDEX, Indexes),

  case {Which, User} of
    {undefined, _} -> [];
    {<<"left">>, CurrentUserId} -> [[1, 0, Which]];
    {<<"right">>, CurrentUserId} -> [[0, 1, Which]];
    {<<"left">>, _} -> [[1, 0, <<"">>]];
    {<<"right">>, _} -> [[0, 1, <<"">>]]
  end.

reduce_count_votes(Vals, _Arg) ->
  lists:foldl(fun reduce_count/2, [0, 0, <<"">>], Vals).
  
reduce_count([L1, R1, <<"">>], [L2, R2, W2]) ->
  [L1 + L2, R1 + R2, W2];
reduce_count([L1, R1, W1], [L2, R2, <<"">>]) ->
  [L1 + L2, R1 + R2, W1].
