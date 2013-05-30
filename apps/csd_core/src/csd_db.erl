-module(csd_db).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([get_snippet/1, save_snippet/1, list_snippets/4]).
-export([get_user/1, save_user/1]).
-export([get_vote/1, save_vote/1, vote_count_for_snippet/1, vote_count_for_snippet/2]).
-export([get_comment/1, save_comment/1, list_all_comments/1, list_comments_since/2]).

%% --------------------------------------------------------------------------------------
%% Snippet API Function Definitions
%% --------------------------------------------------------------------------------------

save_snippet(Snippet) ->
  pooler:use_member(fun(RiakPid) -> csd_snippet_store:save(RiakPid, Snippet) end).

get_snippet(SnippetKey) ->
  pooler:use_member(fun(RiakPid) -> csd_snippet_store:fetch(RiakPid, SnippetKey) end).

list_snippets(UserId, FieldsToKeep, PageSize, PageNumber) ->
  pooler:use_member(fun(RiakPid) ->
        csd_snippet_store:list_for_user(RiakPid, UserId, FieldsToKeep, PageSize, PageNumber)
    end).

%% --------------------------------------------------------------------------------------
%% Comment API Function Definitions
%% --------------------------------------------------------------------------------------

get_comment(CommentId) ->
  pooler:use_member(fun(RiakPid) -> csd_comment_store:fetch(RiakPid, CommentId) end).

save_comment(Comment) ->
  pooler:use_member(fun(RiakPid) -> csd_comment_store:save(RiakPid, Comment) end).

list_all_comments(SnippetId) ->
  pooler:use_member(fun(RiakPid) -> csd_comment_store:list_all(RiakPid, SnippetId) end).

list_comments_since(SnippetId, Since) ->
  pooler:use_member(fun(RiakPid) -> csd_comment_store:list_all_since(RiakPid, SnippetId, Since) end).

%% --------------------------------------------------------------------------------------
%% User API Function Definitions
%% --------------------------------------------------------------------------------------

get_user(UserId) ->
  pooler:use_member(fun(RiakPid) -> csd_user_store:fetch(RiakPid, UserId) end).

save_user(User) ->
  pooler:use_member(fun(RiakPid) -> csd_user_store:save(RiakPid, User) end).

%% --------------------------------------------------------------------------------------
%% Vote API Function Definitions
%% --------------------------------------------------------------------------------------

get_vote(VoteId) ->
  pooler:use_member(fun(RiakPid) -> csd_vote_store:fetch(RiakPid, VoteId) end).

save_vote(Vote) ->
  pooler:use_member(fun(RiakPid) -> csd_vote_store:save(RiakPid, Vote) end).

vote_count_for_snippet(SnippetId) ->
   pooler:use_member(fun(RiakPid) ->
        csd_vote_store:count_for_snippet(RiakPid, SnippetId)
    end).

vote_count_for_snippet(SnippetId, UserId) ->
  pooler:use_member(fun(RiakPid) ->
        csd_vote_store:count_for_snippet(RiakPid, SnippetId, UserId)
    end).

