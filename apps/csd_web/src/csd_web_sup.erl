%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

%% @doc Supervisor for the csd_web application.

-module(csd_web_sup).
-author('OJ Reeves <oj@buffered.io>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
      supervisor:terminate_child(?MODULE, Id),
      supervisor:delete_child(?MODULE, Id),
      ok
    end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  WebConfig = conf:get_section(web),
  Web = {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, dynamic},
  Processes = [Web],
  {ok, { {one_for_one, 10, 10}, Processes} }.
