%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_view).

-author('OJ Reeves <oj@buffered.io>').

%-export([logon/0, logged_on/1, logged_on/2).
-export([logon/0, home/0, home/1]).

home() ->
  Params = [{logged_in, false}, {logon_url, "/oauth/logon"}],
  {ok, Content} = home_dtl:render(Params),
  Content.

home(Name) ->
  Params = [{logged_in, true}, {user_name, Name}],
  {ok, Content} = home_dtl:render(Params),
  Content.

logon() ->
  Params = [{request_url, conf:get_val(urimap, twitter_logon)}],
  {ok, Content} = logon_dtl:render(Params),
  Content.
