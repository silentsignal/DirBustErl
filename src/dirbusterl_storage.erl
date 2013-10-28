-module(dirbusterl_storage).
-export([init_schema/0, allocate_bust_id/2, store_finding/3, get_findings/1]).

-record(dirbusterl_bust, {id, url, config}).
-record(dirbusterl_finding, {bust_id_url, metadata}).

allocate_bust_id(URL, Config) ->
	Id = now(),
	{atomic, ok} = mnesia:transaction(fun () ->
		mnesia:write(#dirbusterl_bust{id=Id, url=URL, config=Config})
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
