-module(dirbusterl_storage).
-export([init_schema/0, allocate_bust_id/2, store_finding/3, get_findings/1, set_server_pid/2, get_server_pid/1, get_busts/0]).

-record(dirbusterl_bust, {id, url, config, server_pid=not_started}).
-record(dirbusterl_finding, {bust_id_url, metadata}).

-include_lib("stdlib/include/qlc.hrl").

allocate_bust_id(URL, Config) ->
	Id = now(),
	{atomic, ok} = mnesia:transaction(fun () ->
		mnesia:write(#dirbusterl_bust{id=Id, url=list_to_binary(URL), config=Config})
	end),
	Id.

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
	Findings.

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
			{id, string_id(R#dirbusterl_bust.id)},
			{url, R#dirbusterl_bust.url}] || R <- mnesia:table(dirbusterl_bust)]))
	end),
	Busts.

string_id(Id) -> base64:encode(term_to_binary(Id)).
