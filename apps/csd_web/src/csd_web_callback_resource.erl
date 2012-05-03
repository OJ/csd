-module(csd_web_callback_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([init/1, resource_exists/2, previously_existed/2, moved_temporarily/2]).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([]) ->
  {ok, undefined}.

resource_exists(ReqData, State) ->
  {false, ReqData, State}.

previously_existed(ReqData, State) ->
  {true, ReqData, State}.

moved_temporarily(ReqData, State) ->
  ReqToken = wrq:get_qs_value("oauth_token", ReqData),
  ReqTokenSecret = wrq:get_qs_value("oauth_token_secret", ReqData),
  Verifier = wrq:get_qs_value("oauth_verifier", ReqData),

  {ok, AccessToken, AccessTokenSecret} = twitter:verify_access(ReqToken, ReqTokenSecret, Verifier),
  {ok, UserInfoJson} = twitter:get_current_user_info(AccessToken, AccessTokenSecret),
  {struct, Json} = mochijson2:decode(UserInfoJson),
  UserId = proplists:get_value(<<"id">>, Json),
  UserName = proplists:get_value(<<"screen_name">>, Json),
  NewReqData = cookie:store_auth(ReqData, UserId, UserName, AccessToken, AccessTokenSecret),

  User = csd_user:to_user(UserId, UserName),
  %io:format("~p~n", [User]),
  {ok, _} = csd_user:save(User),

  % TODO: error handlng for when things don't go to plan
  {{true, conf:get_val(urimap, home)}, NewReqData, State}.

