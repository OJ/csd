-module(csd_web_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

to_html(ReqData, State) ->
  Content = case cookie:load_auth(ReqData) of
    {ok, {UserId, _, _, _}} ->
      csd_view:home(UserId);
    _ ->
      csd_view:home()
  end,
  {Content, ReqData, State}.

