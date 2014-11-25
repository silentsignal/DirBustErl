-module(bust_resource).
-export([init/1, content_types_provided/2, delete_resource/2, to_json/2, content_types_accepted/2, allowed_methods/2, from_json/2, post_is_create/2, create_path/2]).

-include_lib("webmachine/include/webmachine.hrl").


init([Format]) -> {ok, Format}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

allowed_methods(ReqData, State) ->
    {['HEAD', 'GET', 'POST', 'PUT', 'DELETE'], ReqData, State}.

post_is_create(ReqData, State) ->
    {true, ReqData, State}.

create_path(R, S) ->
    {usb64:encode_to_string(dirbusterl_storage:generate_bust_id()), R, S}.

delete_resource(ReqData, State) ->
    {dirbusterl_storage:delete_bust(get_bust_id(ReqData)), ReqData, State}.

to_json(ReqData, State) ->
    case get_json_data(get_bust_id(ReqData), State) of
        not_found -> {halt, 404};
        Payload -> {mochijson2:encode(Payload), ReqData, State}
    end.

get_bust_id(ReqData) ->
    usb64:decode(proplists:get_value(bust_id, wrq:path_info(ReqData), "")).

get_json_data(_, list) -> dirbusterl_storage:get_busts();
get_json_data(BustId, status) -> dirbusterl_storage:get_bust_status(BustId);
get_json_data(BustId, findings) -> dirbusterl_storage:get_findings(BustId).

from_json(ReqData, State = status) ->
    {struct, Values} = mochijson2:decode(wrq:req_body(ReqData)),
    case {Values, dirbusterl_storage:get_server_pid(get_bust_id(ReqData))} of
        {[{<<"status">>, <<"aborted">>}], Pid} when is_pid(Pid) ->
            exit(Pid, user_abortion),
            {true, ReqData, State};
        _ ->
            {halt, 501} %% Not Implemented
    end;
from_json(ReqData, State = list) ->
    Id = usb64:decode(wrq:disp_path(ReqData)),
    {struct, Values} = mochijson2:decode(wrq:req_body(ReqData)),
    {URL, Config} = process_json_values(Values),
    dirbusterl:bust_async(Id, URL, Config),
    {true, ReqData, State}.

-define(AK(X), Key =:= <<X>>).
-define(ALLOWED_KEY,
        ?AK("follow_dirs") ;
        ?AK("follow_redirs") ;
        ?AK("parse_body") ;
        ?AK("url_restriction") ;
        ?AK("postfix") ;
        ?AK("mangle_found") ;
        ?AK("http_cfg") ;
        ?AK("proxy_host") ;
        ?AK("proxy_port") ; %% TODO add more ibrowse atoms
        ?AK("url_list")).

process_json_values(Values) -> process_json_values(Values, no_url, []).
process_json_values([], URL, CfgAcc) -> {URL, CfgAcc};
process_json_values([{<<"url">>, URL} | Values], _, CfgAcc) ->
    process_json_values(Values, binary_to_list(URL), CfgAcc);
process_json_values([{<<"wordlist">>, WordList} | Values], URL, CfgAcc) ->
    true = ordsets:is_element(WordList, wordlist_resource:wordlists()), %% XXX
    WLfile = binary_to_list(filename:join(wordlist_resource:wordlist_dir(), WordList)),
    process_json_values(Values, URL, [{wordlist, WLfile} | CfgAcc]);
process_json_values([{<<"basic_auth">>, [Username, Password]} | Values], URL, CfgAcc) ->
	Creds = {binary_to_list(Username), binary_to_list(Password)},
	process_json_values(Values, URL, [{basic_auth, Creds} | CfgAcc]);
process_json_values([{<<"headers">>, Headers} | Values], URL, CfgAcc) ->
	Tuples = lists:map(fun erlang:list_to_tuple/1, process_json_value(Headers)),
	process_json_values(Values, URL, [{headers, Tuples} | CfgAcc]);
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
process_json_value(Value) when is_integer(Value) -> Value;
process_json_value(Value) when is_list(Value) ->
    lists:map(fun process_json_value/1, Value).
