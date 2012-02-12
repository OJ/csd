%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(cookie).

-author('OJ Reeves <oj@buffered.io>').

-export([auth_required_json/0]).
-export([load_auth/1, store_auth/5]).

-define(AUTH_COOKIE, "__CodeSmackdown_Auth").
-define(AUTH_SALT, "27ed2d041cdb4b8b2702").
-define(AUTH_SECRET, "2d0431cd9bda5ba4b98271edcb2e7102").
-define(AUTH_EXPIRY_DAYS, 7).
-define(ENC_IV, <<207,94,217,158,198,63,132,205,35,187,246,2,56,122,250,33>>).
-define(ENC_KEY,
  <<110,56,121,28,235,159,77,154,160,5,130,210,204,32,26,224,255,86,101,71,61,3,
  66,69,30,39,42,0,116,93,204,99>>).

auth_required_json() ->
  Json = {struct, {"error", "unauthorized"}},
  mochijson2:encode(Json).

load_auth(ReqData) ->
  case wrq:get_cookie_value(?AUTH_COOKIE, ReqData) of
    undefined ->
      {error, no_cookie};
    V ->
      Val = mochiweb_util:unquote(V),
      decode(Val)
  end.

store_auth(ReqData, Id, Name, Token, TokenSecret) ->
  Value = mochiweb_util:quote_plus(encode(Id, Name, Token, TokenSecret)),
  Options = [
    %{domain, "codesmackdown.com"},
    {max_age, 3600 * 24 * ?AUTH_EXPIRY_DAYS},
    {path, "/"},
    {http_only, true}
  ],
  CookieHeader = mochiweb_cookies:cookie(?AUTH_COOKIE, Value, Options),
  wrq:merge_resp_headers([CookieHeader], ReqData).

decode(CookieValue) ->
  {Value={Id, Name, Expire, SecretInfo}, Salt, Sign} = binary_to_term(base64:decode(CookieValue)),
  case crypto:sha_mac(?AUTH_SECRET, term_to_binary([Value, Salt])) of
    Sign ->
      case Expire >= calendar:local_time() of
        true ->
          {Token, TokenSecret} = decrypt(SecretInfo),
          {ok, {Id, Name, Token, TokenSecret}};
        false ->
          {error, expired}
      end;
    _ ->
      {error, invalid}
  end.

encode(Id, Name, Token, TokenSecret) ->
  SecretInfo = encrypt({Token, TokenSecret}),
  CookieValue = {Id, Name, get_expiry(), SecretInfo},
  base64:encode(term_to_binary({CookieValue, ?AUTH_SALT, crypto:sha_mac(?AUTH_SECRET, term_to_binary([CookieValue, ?AUTH_SALT]))})).

get_expiry() ->
  {Date, Time} = calendar:local_time(),
  NewDate = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + ?AUTH_EXPIRY_DAYS),
  {NewDate, Time}.

encrypt(Value) ->
  crypto:aes_ctr_encrypt(?ENC_KEY, ?ENC_IV, term_to_binary([Value, ?AUTH_SALT])).

decrypt(Value) ->
  [V, ?AUTH_SALT] = binary_to_term(crypto:aes_ctr_decrypt(?ENC_KEY, ?ENC_IV, Value)),
  V.
