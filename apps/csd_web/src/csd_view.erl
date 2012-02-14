%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_view).

-author('OJ Reeves <oj@buffered.io>').

-export([home/0, home/1]).

home() ->
  Params = [{logged_in, false}, {logon_url, conf:get_val(urimap, twitter_logon)}],
  {ok, Content} = home_dtl:render(Params),
  Content.

home(Name) ->
  Params = [{logged_in, true}, {user_name, Name}],
  {ok, Content} = home_dtl:render(Params),
  Content.
