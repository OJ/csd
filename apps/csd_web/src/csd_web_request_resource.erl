-module(csd_web_request_resource).
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
  {ok, Url} = twitter:request_access(),
  {{true, Url}, ReqData, State}.

