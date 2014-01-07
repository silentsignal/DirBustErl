-module(bust_resource).
-export([init/1, content_types_provided/2, to_json/2, content_types_accepted/2, allowed_methods/2, from_json/2, post_is_create/2, create_path/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(CT_JSON, {"application/json", to_json}).


init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[?CT_JSON], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

allowed_methods(ReqData, State) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, State}.

post_is_create(ReqData, State) ->
    {true, ReqData, State}.

create_path(R, S) ->
    {base64:encode_to_string(dirbusterl_storage:generate_bust_id()), R, S}.

to_json(ReqData, State) ->
    Payload = case wrq:disp_path(ReqData) of
        "" -> dirbusterl_storage:get_busts();
        BustId -> dirbusterl_storage:get_findings(base64:decode(BustId))
    end,
    {mochijson2:encode(Payload), ReqData, State}.

from_json(ReqData, State) ->
    Id = base64:decode(wrq:disp_path(ReqData)),
    {struct, Values} = mochijson2:decode(wrq:req_body(ReqData)),
    case {Values, dirbusterl_storage:get_server_pid(Id)} of
        {[{<<"status">>, <<"aborted">>}], Pid} when is_pid(Pid) -> exit(Pid, user_abortion);
        {_, not_registered} ->
            {URL, Config} = process_json_values(Values),
            dirbusterl:bust_async(Id, URL, Config)
    end,
    {true, ReqData, State}.

-define(AK(X), Key =:= X).
-define(ALLOWED_KEY,
        ?AK(<<"follow_dirs">>) ;
        ?AK(<<"follow_redirs">>) ;
        ?AK(<<"parse_body">>) ;
        ?AK(<<"url_restriction">>) ;
        ?AK(<<"postfix">>) ;
        ?AK(<<"mangle_found">>) ;
        ?AK(<<"http_cfg">>) ;
        ?AK(<<"proxy_host">>) ;
        ?AK(<<"proxy_port">>) ; %% TODO add more ibrowse atoms
        ?AK(<<"url_list">>)).

process_json_values(Values) -> process_json_values(Values, no_url, []).
process_json_values([], URL, CfgAcc) -> {URL, CfgAcc};
process_json_values([{<<"url">>, URL} | Values], _, CfgAcc) ->
    process_json_values(Values, binary_to_list(URL), CfgAcc);
process_json_values([{<<"wordlist">>, WordList} | Values], URL, CfgAcc) ->
    true = ordsets:is_element(WordList, wordlist_resource:wordlists()), %% XXX
    WLfile = binary_to_list(filename:join(wordlist_resource:wordlist_dir(), WordList)),
    process_json_values(Values, URL, [{wordlist, WLfile} | CfgAcc]);
process_json_values([{Key, Value} | Values], URL, CfgAcc) when ?ALLOWED_KEY ->
    EntryKey = list_to_atom(binary_to_list(Key)),
    Config = case Value of
        true -> [EntryKey | CfgAcc];
        false -> CfgAcc;
        _ -> [{EntryKey, process_json_value(Value)} | CfgAcc]
    end,
    process_json_values(Values, URL, Config).

process_json_value({struct, Values}) ->
    {no_url, Processed} = process_json_values(Values),
    Processed;
process_json_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
process_json_value(Value) when is_list(Value) ->
    lists:map(fun process_json_value/1, Value).
