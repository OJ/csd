-module(csd_core_server).
-author('OJ Reeves <oj@buffered.io>').

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_snippet/1, save_snippet/1, list_snippets/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_snippet(Snippet) ->
  gen_server:call(?SERVER, {save_snippet, Snippet}, infinity).

get_snippet(SnippetKey) ->
  gen_server:call(?SERVER, {get_snippet, SnippetKey}, infinity).

list_snippets(UserId) ->
  gen_server:call(?SERVER, {list_snippets, UserId}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, undefined}.

handle_call({save_snippet, Snippet}, _From, State) ->
  SavedSnippet = pooler:use_member(fun(RiakPid) -> csd_snippet_store:save(RiakPid, Snippet) end),
  {reply, SavedSnippet, State};

handle_call({get_snippet, SnippetKey}, _From, State) ->
  Snippet = pooler:use_member(fun(RiakPid) -> csd_snippet_store:fetch(RiakPid, SnippetKey) end),
  {reply, Snippet, State};

handle_call({list_snippets, UserId}, _From, State) ->
  Snippet = pooler:use_member(fun(RiakPid) -> csd_snippet_store:list_for_user(RiakPid, UserId) end),
  {reply, Snippet, State};

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

