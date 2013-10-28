%% Copyright (c) 2011 James Yu
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(ece_resource_static).

-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").

-record(context, {docroot,fullpath,fileinfo,response_body}).

init([]) ->
    {ok, App}= application:get_application(),
    PrivDir = filename:join(code:priv_dir(App), "static"),
    {ok, #context{docroot=PrivDir}}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    Path = get_full_path(Ctx, wrq:disp_path(ReqData)),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Ctx}.

provide_content(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            {Body, ReqData, Context};
        {false, NewContext} ->
            {error, ReqData, NewContext}
    end.

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
        undefined ->
            case file_exists(Context, Path) of
                {true, FullPath} ->
                    {ok, Value} = file:read_file(FullPath),
                    {true, Context#context{response_body=Value}};
                false ->
                    {false, Context}
            end;
        _Body ->
            {true, Context}
    end.

file_exists(Context, Path) ->
    FPath = get_full_path(Context, Path),
    case filelib:is_regular(filename:absname(FPath)) of
        true ->
            {true, FPath};
        false ->
            false
    end.

get_full_path(Context, Path) ->
    Root = Context#context.docroot,
    case mochiweb_util:safe_relative_path(Path) of
        undefined -> undefined;
        RelPath ->
            FullPath = filename:join([Root, RelPath]),
            case filelib:is_dir(FullPath) of
                true ->
                    filename:join([FullPath, "index.html"]);
                false ->
                    FullPath
            end
    end.
