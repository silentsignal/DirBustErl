-module(dirbusterl_storage).
-export([init_schema/0, generate_bust_id/0, register_bust/3, delete_bust/1, store_finding/3, get_findings/1, set_server_pid/2, get_server_pid/1, get_busts/0]).

-record(dirbusterl_bust, {id, url, config, server_pid=not_started}).
-record(dirbusterl_finding, {bust_id_url, metadata}).

-include_lib("stdlib/include/qlc.hrl").

generate_bust_id() -> term_to_binary(now()).

register_bust(Id, URL, Config) ->
	{atomic, ok} = mnesia:transaction(fun () ->
		mnesia:write(#dirbusterl_bust{id=Id, url=list_to_binary(URL), config=Config})
	end).

delete_bust(BustId) ->
    {atomic, Result} = mnesia:transaction(fun () ->
        mnesia:delete({dirbusterl_bust, BustId}),
        lists:foreach(fun mnesia:delete_object/1, mnesia:match_object(
            #dirbusterl_finding{bust_id_url={BustId, '_'}, metadata='_'})),
        true %% TODO handle busts that are still running
    end),
    Result.

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
	{atomic, Busts} = mnesia:transaction(fun () ->
		mnesia:read({dirbusterl_bust, BustId})
	end),
	case Busts of
		[Bust] -> Bust#dirbusterl_bust.server_pid;
		[] -> not_registered
	end.

get_busts() ->
	{atomic, Busts} = mnesia:transaction(fun() ->
		qlc:e(qlc:q([[
			{id, usb64:encode(R#dirbusterl_bust.id)},
			{started, bust_id_to_datetime(R#dirbusterl_bust.id)},
			{url, R#dirbusterl_bust.url}
			| (format_bust_status(R#dirbusterl_bust.server_pid) ++
				config_value_to_json(R#dirbusterl_bust.config))]
			|| R <- mnesia:table(dirbusterl_bust)]))
	end),
	Busts.

format_bust_status(ServerPid) ->
	[{status, bust_status(ServerPid)}, {ended, decode_ended(ServerPid)},
		{requests, dirbusterl_requests:get_value(ServerPid)}].

decode_ended(ServerPid) when is_tuple(ServerPid),
		element(1, ServerPid) =:= finished ->
	timestamp_to_datetime(element(2, ServerPid));
decode_ended(_) -> null.

bust_id_to_datetime(BustId) ->
	timestamp_to_datetime(binary_to_term(BustId)).

timestamp_to_datetime(TimeStamp) ->
	{{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_local_time(TimeStamp),
	list_to_binary(io_lib:format(
		"~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		[Y, Mo, D, H, Mi, S])).

bust_status(not_started) -> <<"not_started">>;
bust_status({finished, _, normal}) -> <<"finished">>;
bust_status({finished, _, Info}) ->
	unicode:characters_to_binary((io_lib:format("~p", [Info])), latin1, utf8);
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
config_to_json({headers, Tuples}) ->
	{headers, config_value_to_json(lists:map(fun tuple_to_list/1, Tuples))};
config_to_json({Atom, Value}) ->
	{Atom, config_value_to_json(Value)};
config_to_json(Atom) when is_atom(Atom) ->
	{Atom, true}.

config_value_to_json([] = EmptyList) -> EmptyList;
config_value_to_json([Elem | _] = Value) when is_integer(Elem) ->
	list_to_binary(Value);
config_value_to_json([Elem | _] = Value) when is_list(Elem) ->
	lists:map(fun config_value_to_json/1, Value);
config_value_to_json([Elem | _] = Value) when is_tuple(Elem); is_atom(Elem) ->
	lists:map(fun config_to_json/1, Value).
