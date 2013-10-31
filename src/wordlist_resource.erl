-module(wordlist_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(CT_JSON, {"application/json", to_json}).


init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[?CT_JSON], ReqData, State}.

to_json(ReqData, State) ->
    {ok, Files} = file:list_dir(filename:join(code:priv_dir("dirbusterl"), "wordlists")),
    WLs = lists:sort(Files -- [".gitignore"]),
    {mochijson2:encode(lists:map(fun list_to_binary/1, WLs)), ReqData, State}.
