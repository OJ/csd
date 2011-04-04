-module(csd_web_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

to_html(ReqData, State) ->
  {ok, Content} = sample_dtl:render([{param, "Slartibartfast"}]),
  {Content, ReqData, State}.

