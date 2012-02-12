%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2012 OJ Reeves

-module(csd_web_logon_resource).

-author('OJ Reeves <oj@buffered.io>').

-export([init/1, to_html/2, resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, false}.

resource_exists(ReqData, _State) ->
  % determine if the user is already logged on and pass that on to the
  % 'moved_temporarily()'
  LoggedOn = case cookie:load_auth(ReqData) of
    {ok, _} ->
      true;
    {error, _} ->
      false
  end,

  {not LoggedOn, ReqData, LoggedOn}.

previously_existed(ReqData, State) ->
  {State, ReqData, State}.

moved_temporarily(ReqData, State) ->
  Response = case State of
    true ->
      % already logged on, so redirect the user to the 'home' page
      {true, conf:get_val(urimap, home)};
    _ ->
      % not logged on, so allow them to move on to 'to_html()'
      false
  end,
  {Response, ReqData, State}.

to_html(ReqData, State) ->
  Content = csd_view:logon(),
  {Content, ReqData, State}.
