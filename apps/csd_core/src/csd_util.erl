-module(csd_util).
-author('OJ Reeves <oj@buffered.io>').

-define(EPOCH_DIFF, 62167219200).

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([utc_now/0, utc/1, epoch_utc_now/0, epoch_utc/1, to_binary/1]).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

utc_now() ->
  utc(erlang:now()).

utc(Now = {_, _, Micro}) ->
  {{Y, M, D}, {H, MM, S}} = calendar:now_to_universal_time(Now),
  iolist_to_binary(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w.~3.3.0wZ", [Y, M, D, H, MM, S, trunc(Micro / 1000)])).

epoch_utc_now() ->
  erlang:now().

epoch_utc(Now) ->
  calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Now)) - ?EPOCH_DIFF.

to_binary(X) when is_atom(X) ->
  to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) ->
  to_binary(integer_to_list(X));
to_binary(X) when is_list(X) ->
  list_to_binary(X);
to_binary(X) when is_binary(X) ->
  X.
