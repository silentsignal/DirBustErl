-module(wordlist_resource).
-export([init/1, content_types_provided/2, to_json/2, wordlist_dir/0, wordlists/0]).

-include_lib("webmachine/include/webmachine.hrl").
-define(CT_JSON, {"application/json", to_json}).


init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[?CT_JSON], ReqData, State}.

wordlist_dir() -> filename:join(code:priv_dir("dirbusterl"), "wordlists").

wordlists() ->
    {ok, Files} = file:list_dir(wordlist_dir()),
    lists:foldl(fun (".gitignore", A) -> A;
                    (E, A) -> ordsets:add_element(list_to_binary(E), A) end,
                ordsets:new(), Files).

to_json(ReqData, State) ->
    {mochijson2:encode(wordlists()), ReqData, State}.
