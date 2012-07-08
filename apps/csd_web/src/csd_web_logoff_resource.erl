-module(csd_web_logoff_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([init/1,
    allowed_methods/2,
    process_post/2
  ]).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([]) ->
  {{trace, "/tmp"}, undefined}.

allowed_methods(ReqData, State) ->
  {['POST'], ReqData, State}.

process_post(ReqData, State) ->
  NewReqData = cookie:remove_auth(ReqData),
  {true, NewReqData, State}.
