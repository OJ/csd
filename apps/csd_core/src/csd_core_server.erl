-module(csd_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_snippet/1, save_snippet/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  ConnInfo = csd_riak_config:connection_info(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConnInfo], []).

save_snippet(Snippet) ->
  gen_server:call(?SERVER, {save_snippet, Snippet}, infinity).

get_snippet(SnippetKey) ->
  gen_server:call(?SERVER, {get_snippet, SnippetKey}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ConnInfo]) ->
  {ok, ConnInfo}.

handle_call({save_snippet, Snippet}, _From, ConnInfo) ->
  RiakPid = csd_riak:connect(ConnInfo),
  SavedSnippet = csd_snippet:save(RiakPid, Snippet),
  {reply, SavedSnippet, ConnInfo};

handle_call({get_snippet, SnippetKey}, _From, ConnInfo) ->
  RiakPid = csd_riak:connect(ConnInfo),
  Snippet = csd_snippet:fetch(RiakPid, SnippetKey),
  {reply, Snippet, ConnInfo};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

