-module(csd_core_server).
-author('OJ Reeves <oj@buffered.io>').

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([get_snippet/1, save_snippet/1, list_snippets/1]).
-export([get_user/1, save_user/1]).
-export([get_vote/1, save_vote/1, vote_count_for_snippet/1, vote_count_for_snippet/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Snippet API Function Definitions
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
%% User API Function Definitions
%% ------------------------------------------------------------------

get_user(UserId) ->
  gen_server:call(?SERVER, {get_user, UserId}, infinity).

save_user(User) ->
  gen_server:call(?SERVER, {save_user, User}, infinity).

%% ------------------------------------------------------------------
%% Vote API Function Definitions
%% ------------------------------------------------------------------

get_vote(VoteId) ->
  gen_server:call(?SERVER, {get_vote, VoteId}, infinity).

save_vote(Vote) ->
  gen_server:call(?SERVER, {save_vote, Vote}, infinity).

vote_count_for_snippet(SnippetId) ->
  gen_server:call(?SERVER, {vote_count_for_snippet, SnippetId}, infinity).

vote_count_for_snippet(SnippetId, UserId) ->
  gen_server:call(?SERVER, {vote_count_for_snippet, SnippetId, UserId}, infinity).

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

handle_call({save_user, User}, _From, State) ->
  SavedUser = pooler:use_member(fun(RiakPid) -> csd_user_store:save(RiakPid, User) end),
  {reply, SavedUser, State};

handle_call({get_user, UserId}, _From, State) ->
  User = pooler:use_member(fun(RiakPid) -> csd_user_store:fetch(RiakPid, UserId) end),
  {reply, User, State};

handle_call({get_vote, VoteId}, _From, State) ->
  Vote = pooler:use_member(fun(RiakPid) -> csd_vote_store:fetch(RiakPid, VoteId) end),
  {reply, Vote, State};

handle_call({save_vote, Vote}, _From, State) ->
  SavedVote = pooler:use_member(fun(RiakPid) -> csd_vote_store:save(RiakPid, Vote) end),
  {reply, SavedVote, State};

handle_call({vote_count_for_snippet, SnippetId}, _From, State) ->
  VoteCount = pooler:use_member(fun(RiakPid) ->
        csd_vote_store:count_for_snippet(RiakPid, SnippetId)
    end),
  {reply, VoteCount, State};

handle_call({vote_count_for_snippet, SnippetId, UserId}, _From, State) ->
  VoteCount = pooler:use_member(fun(RiakPid) ->
        csd_vote_store:count_for_snippet(RiakPid, SnippetId, UserId)
    end),
  {reply, VoteCount, State};

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

