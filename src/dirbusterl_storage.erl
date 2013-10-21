-module(dirbusterl_storage).
-export([init_schema/0, allocate_bust_id/2]).

-record(dirbusterl_bust, {id, url, config}).

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
	ok = mnesia:wait_for_tables([dirbusterl_bust], 5000).
