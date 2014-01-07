-module(dirbusterl_storage).
-export([init_schema/0, generate_bust_id/0, register_bust/3, store_finding/3, get_findings/1, set_server_pid/2, get_server_pid/1, get_busts/0]).

-record(dirbusterl_bust, {id, url, config, server_pid=not_started}).
-record(dirbusterl_finding, {bust_id_url, metadata}).

-include_lib("stdlib/include/qlc.hrl").

generate_bust_id() -> term_to_binary(now()).

register_bust(Id, URL, Config) ->
	{atomic, ok} = mnesia:transaction(fun () ->
		mnesia:write(#dirbusterl_bust{id=Id, url=list_to_binary(URL), config=Config})
	end).

init_schema() ->
	case mnesia:create_table(dirbusterl_bust, [
		{attributes, record_info(fields, dirbusterl_bust)}, {disc_copies, [node()]}]) of
		{atomic, ok} -> ok;
		{aborted, {already_exists, dirbusterl_bust}} -> ok
	end,
	case mnesia:create_table(dirbusterl_finding, [
		{attributes, record_info(fields, dirbusterl_finding)}, {disc_copies, [node()]}]) of
		{atomic, ok} -> ok;
		{aborted, {already_exists, dirbusterl_finding}} -> ok
	end,
	ok = mnesia:wait_for_tables([dirbusterl_bust, dirbusterl_finding], 5000).

store_finding(BustId, URL, Metadata) ->
	{atomic, ok} = mnesia:transaction(fun () ->
		mnesia:write(#dirbusterl_finding{bust_id_url={BustId, URL}, metadata=Metadata})
	end).

get_findings(BustId) ->
    {atomic, Findings} = mnesia:transaction(fun() ->
         mnesia:match_object(#dirbusterl_finding{bust_id_url={BustId, '_'}, metadata='_'})
    end),
	lists:map(fun finding_to_proplist/1, Findings).

finding_to_proplist(#dirbusterl_finding{bust_id_url={_, URL}, metadata=MD}) ->
    [{url, URL} | proplists:unfold(MD)].

set_server_pid(BustId, ServerPid) ->
	{atomic, ok} = mnesia:transaction(fun () ->
		[Bust] = mnesia:wread({dirbusterl_bust, BustId}),
		mnesia:write(Bust#dirbusterl_bust{server_pid=ServerPid})
	end).

get_server_pid(BustId) ->
	{atomic, [Bust]} = mnesia:transaction(fun () ->
		mnesia:read({dirbusterl_bust, BustId})
	end),
	Bust#dirbusterl_bust.server_pid.

get_busts() ->
	{atomic, Busts} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([[
			{id, base64:encode(R#dirbusterl_bust.id)},
			{started, bust_id_to_datetime(R#dirbusterl_bust.id)},
			{status, bust_status(R#dirbusterl_bust.server_pid)},
			{url, R#dirbusterl_bust.url} | config_value_to_json(R#dirbusterl_bust.config)]
			|| R <- mnesia:table(dirbusterl_bust)]))
	end),
	Busts.

bust_id_to_datetime(BustId) ->
	{{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_local_time(binary_to_term(BustId)),
	list_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		[Y, Mo, D, H, Mi, S])).

bust_status(not_started) -> <<"not_started">>;
bust_status({finished, normal}) -> <<"finished">>;
bust_status({finished, Info}) -> list_to_binary(io_lib:format("~p", [Info]));
bust_status(Pid) when is_pid(Pid) ->
	case process_info(Pid) of
		undefined -> <<"Process haven't finished but does not exist.">>;
		_ -> <<"running">>
	end.

config_to_json({wordlist, Value}) ->
    WD = list_to_binary(wordlist_resource:wordlist_dir()),
    VB = list_to_binary(Value),
    CommonPrefix = binary:longest_common_prefix([WD, VB]),
    Payload = case CommonPrefix =:= byte_size(WD) of
        true -> binary_part(VB, CommonPrefix + 1, byte_size(VB) - CommonPrefix - 1);
        false -> VB
    end,
    {wordlist, Payload};
config_to_json({Atom, Value}) ->
	{Atom, config_value_to_json(Value)};
config_to_json(Atom) when is_atom(Atom) ->
	{Atom, true}.

config_value_to_json([Elem | _] = Value) when is_integer(Elem) ->
	list_to_binary(Value);
config_value_to_json([Elem | _] = Value) when is_list(Elem) ->
	lists:map(fun config_value_to_json/1, Value);
config_value_to_json([Elem | _] = Value) when is_tuple(Elem) ->
	lists:map(fun config_to_json/1, Value).
