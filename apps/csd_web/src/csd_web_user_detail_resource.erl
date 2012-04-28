-module(csd_web_user_detail_resource).
-author('OJ Reeves <oj@buffered.io>').

-export([init/1,
    allowed_methods/2,
    content_types_provided/2,
    to_json/2
  ]).

-record(state, {
  }).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #state{}}.

content_types_provided(ReqData, State=#state{}) ->
  Types = [
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

allowed_methods(ReqData, State=#state{}) ->
  {['GET'], ReqData, State}.

to_json(ReqData, State=#state{}) ->
  PathInfo = wrq:path_info(ReqData),
  {ok, UserId} = dict:find(user_id, PathInfo),
  {ok, Snippets} = csd_snippet:list_for_user(UserId),
  UserData = {struct, [
      {user_name, <<"name goes here">>},
      {snippets, Snippets}
    ]},
  Json = mochijson2:encode(UserData),
  {Json, ReqData, State}.
