%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_web_callback_resource).

-author('OJ Reeves <oj@buffered.io>').

-export([init/1, resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

resource_exists(ReqData, State) ->
  {false, ReqData, State}.

previously_existed(ReqData, State) ->
  {true, ReqData, State}.

moved_temporarily(ReqData, State) ->
  handle_callback(ReqData, State).

handle_callback(ReqData, State) ->
  ReqToken = wrq:get_qs_value("oauth_token", ReqData),
  ReqTokenSecret = wrq:get_qs_value("oauth_token_secret", ReqData),
  Verifier = wrq:get_qs_value("oauth_verifier", ReqData),

  {ok, AccessToken, AccessTokenSecret} = twitter:verify_access(ReqToken, ReqTokenSecret, Verifier),
  {ok, UserInfoJson} = twitter:get_current_user_info(AccessToken, AccessTokenSecret),
  {struct, Json} = mochijson2:decode(UserInfoJson),
  UserId = proplists:get_value(<<"id">>, Json),
  UserName = proplists:get_value(<<"screen_name">>, Json),
  NewReqData = cookie:store_auth(ReqData, UserId, UserName, AccessToken, AccessTokenSecret),

  % TODO: store the 'session' in Riak in an ETS backend

  % TODO: error handlng for when things don't go to plan
  {{true, conf:get_val(urimap, home)}, NewReqData, State}.

