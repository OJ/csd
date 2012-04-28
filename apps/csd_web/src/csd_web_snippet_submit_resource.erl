-module(csd_web_snippet_submit_resource).
-author('OJ Reeves <oj@buffered.io>').

-export([init/1,
    allowed_methods/2,
    content_types_accepted/2,
    is_authorized/2,
    process_form/2,
    post_is_create/2,
    create_path/2,
    to_html/2
  ]).

-record(state, {
    user_data = undefined,
    key
  }).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #state{}}.

content_types_accepted(ReqData, State=#state{}) ->
  Types = [
    {"text/html", to_html},
    {"application/x-www-form-urlencoded", process_form}
  ],
  {Types, ReqData, State}.

allowed_methods(ReqData, State=#state{}) ->
  {['GET', 'POST'], ReqData, State}.

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
  NewReqData = wrq:set_resp_header("Content-type", "text/plain", wrq:set_resp_body(Key, ReqData)),
  {true, NewReqData, State}.

to_html(ReqData, State=#state{}) ->
  TemplateData = [
    % post back to the same URL
    {post_url, wrq:path(ReqData)}
  ],
  {ok, Content} = snippet_submit_dtl:render(TemplateData),
  {Content, ReqData, State}.

to_snippet(FormData, #state{key=Key, user_data={UserId, _, _, _}}) ->
  Title = proplists:get_value("title", FormData),
  Left = proplists:get_value("left", FormData),
  Right = proplists:get_value("right", FormData),
  Snippet = csd_snippet:to_snippet(Title, Left, Right, UserId),
  csd_snippet:set_key(Snippet, Key).

