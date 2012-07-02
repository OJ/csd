-module(csd_static_resource).
-author('OJ Reeves <oj@buffered.io>').

%% --------------------------------------------------------------------------------------
%% API Function Exports
%% --------------------------------------------------------------------------------------

-export([
    init/1,
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    provide_content/2,
    file_exists/2
  ]).

%% --------------------------------------------------------------------------------------
%% Required Includes
%% --------------------------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------------------------
%% Record definitions
%% --------------------------------------------------------------------------------------

-record(context, {docroot, fullpath, fileinfo, response_body}).

%% --------------------------------------------------------------------------------------
%% API Function Definitions
%% --------------------------------------------------------------------------------------

init([ContentDir]) ->
  {ok, App}= application:get_application(),
  PrivDir = code:priv_dir(App),
  SourceDir = filename:join([PrivDir, ContentDir]),
  {ok, #context{docroot=SourceDir}}.

allowed_methods(ReqData, Context) ->
  {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
  {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
  Path = wrq:disp_path(ReqData),
  Mime = webmachine_util:guess_mime(Path),
  {[{Mime, provide_content}], ReqData, Ctx}.

provide_content(ReqData, Context) ->
  % if returns {true, NewContext} then NewContext has response_body
  case Context#context.response_body of
    undefined ->
      case file_exists(Context, wrq:disp_path(ReqData)) of
        {true, FullPath} ->
          {ok, Value} = file:read_file(FullPath),
          {Value, ReqData, Context#context{response_body=Value}};
        false ->
          {error, ReqData, Context}
      end;
    _Body ->
      {Context#context.response_body, ReqData, Context}
  end.

file_exists(Context, Path) ->
  FullPath = get_full_path(Context, Path),
  case filelib:is_regular(filename:absname(FullPath)) of
    true ->
      {true, FullPath};
    false ->
      false
  end.

get_full_path(Context, Path) ->
  Root = Context#context.docroot,
  Result = case mochiweb_util:safe_relative_path(Path) of
    undefined ->
      undefined;
    RelPath ->
      FullPath = filename:join([Root, RelPath]),
      case filelib:is_dir(FullPath) of
        true ->
          filename:join([FullPath, "index.html"]);
        false ->
          FullPath
      end
  end,
  Result.

