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
      csd_user:get_name(UserInfo)
  end,

  {ok, {Snippets, Page, Pages}} = csd_snippet:list_for_user(UserId),
  UserData = {[
      {<<"user_name">>, UserName},
      {<<"snippets">>, [{S} || S <- Snippets]},
      {<<"page">>, Page},
      {<<"pages">>, Pages}
    ]},
  Json = jiffy:encode(UserData),
  {Json, ReqData, State}.

