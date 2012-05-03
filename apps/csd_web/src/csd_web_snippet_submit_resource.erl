-module(csd_web_snippet_submit_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    init/1,
    allowed_methods/2,
    content_types_accepted/2,
    is_authorized/2,
    process_form/2,
    post_is_create/2,
    create_path/2
  ]).

%% --------------------------------------------------------------------------------------
%% Internal Record Definitions
%% --------------------------------------------------------------------------------------

-record(state, {
    user_data = undefined,
    key
  }).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([]) ->
  {ok, #state{}}.

content_types_accepted(ReqData, State=#state{}) ->
  Types = [
    {"application/x-www-form-urlencoded", process_form}
  ],
  {Types, ReqData, State}.

allowed_methods(ReqData, State=#state{}) ->
  {['POST'], ReqData, State}.

is_authorized(ReqData, State=#state{}) ->
  case cookie:load_auth(ReqData) of
    {ok, UserData} ->
      {true, ReqData, State#state{user_data=UserData}};
    _ ->
      {false, ReqData, State}
  end.

post_is_create(ReqData, State=#state{}) ->
  {true, ReqData, State}.

create_path(ReqData, State=#state{}) ->
  Key = csd_riak:new_key(),
  Path = "/snippet/" ++ binary_to_list(Key),
  {Path, ReqData, State#state{key=Key}}.

process_form(ReqData, State=#state{}) ->
  io:format("~p~n", [wrq:host_tokens(ReqData)]),
  % get the detail from the form
  FormData = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
  Snippet = to_snippet(FormData, State),
  {ok, SavedSnippet} = csd_snippet:save(Snippet),
  Key = csd_snippet:get_key(SavedSnippet),

  % Return the key of the snippet as the payload
  NewBody = wrq:set_resp_body(Key, ReqData),
  NewReqData = wrq:set_resp_header("Content-type", "text/plain", NewBody),
  {true, NewReqData, State}.

%% --------------------------------------------------------------------------------------
%% Private Function Definitions
%% --------------------------------------------------------------------------------------

to_snippet(FormData, #state{key=Key, user_data={UserId, _, _, _}}) ->
  Title = proplists:get_value("title", FormData),
  Left = proplists:get_value("left", FormData),
  Right = proplists:get_value("right", FormData),
  Snippet = csd_snippet:to_snippet(Title, Left, Right, UserId),
  csd_snippet:set_key(Snippet, Key).

