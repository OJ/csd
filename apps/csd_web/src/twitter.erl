%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(twitter).

-author('OJ Reeves <oj@buffered.io>').

-export([request_access/0, verify_access/3, get_current_user_info/2]).

request_access() ->
  TwitterConf = conf:get_section(twitter),
  RequestTokenUrl = proplists:get_value(request_token_url, TwitterConf),
  {ok, RequestResponse} = oauth:get(RequestTokenUrl, [], consumer(TwitterConf)),
  RequestParams = oauth:params_decode(RequestResponse),
  RequestToken = oauth:token(RequestParams),
  AuthenticateUrl = proplists:get_value(authenticate_url, TwitterConf),
  {ok, oauth:uri(AuthenticateUrl, [{"oauth_token", RequestToken}])}.

verify_access(RequestToken, RequestTokenSecret, Verifier) ->
  TwitterConf = conf:get_section(twitter),
  AccessTokenUrl = proplists:get_value(access_token_url, TwitterConf),
  {ok, AccessResponse} = oauth:get(AccessTokenUrl, [{"oauth_verifier", Verifier}], consumer(TwitterConf), RequestToken, RequestTokenSecret),
  AccessParams = oauth:params_decode(AccessResponse),
  AccessToken = oauth:token(AccessParams),
  AccessTokenSecret = oauth:token_secret(AccessParams),
  {ok, AccessToken, AccessTokenSecret}.

get_current_user_info(AccessToken, AccessTokenSecret) ->
  call_json_service(current_user_info_url, AccessToken, AccessTokenSecret).

%% Extract a oauth-formatted consumer tuple from the given Twitter configuration.
consumer(TwitterConf) ->
  ConsumerKey = proplists:get_value(consumer_key, TwitterConf),
  ConsumerSecret = proplists:get_value(consumer_secret, TwitterConf),
  {ConsumerKey, ConsumerSecret, hmac_sha1}.

%% Invoke a call to a JSON service on Twitter.
call_json_service(UrlKey, AccessToken, AccessTokenSecret) ->
  TwitterConf = conf:get_section(twitter),
  Url = proplists:get_value(UrlKey, TwitterConf),
  {ok, Response} = oauth:get(Url, [], consumer(TwitterConf), AccessToken, AccessTokenSecret),
  {{_Version, 200, "OK"}, _Headers, Json} = Response,
  {ok, Json}.
