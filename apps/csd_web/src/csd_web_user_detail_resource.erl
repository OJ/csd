-module(csd_web_user_detail_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    init/1,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
  ]).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, State) ->
  Types = [
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

allowed_methods(ReqData, State) ->
  {['GET'], ReqData, State}.

to_json(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, UserId} = dict:find(user_id, PathInfo),

  % We need to render a username, but don't hit the DB
  % if the user is the same as the one looking at the
  % page.
  UserName = case cookie:load_auth(ReqData) of
    {ok, {UserId, Name, _, _}} ->
      Name;
    _ ->
      {ok, UserInfo} = csd_user:fetch(UserId),
      list_to_binary(csd_user:get_name(UserInfo))
  end,

  {ok, Snippets} = csd_snippet:list_for_user(UserId),
  UserData = {struct, [
      {user_name, UserName},
      {snippets, Snippets}
    ]},
  Json = mochijson2:encode(UserData),
  {Json, ReqData, State}.

