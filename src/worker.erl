-module(worker).
-export([try_url/5, try_url_sync/3]).

-include_lib("eunit/include/eunit.hrl").

-define(TRIES, 8).

try_url(URL, Waiter, Headers, Params, FailCase) ->
	case try_url_sync(URL, Headers, Params) of
		not_found -> waiter:worker_finished(Waiter);
		FailCase -> waiter:worker_finished(Waiter);
		{Result, Payload} -> waiter:worker_finished(Waiter, URL, Result, Payload)
	end.

try_url_sync(URL, Headers, Params) -> try_url_sync(URL, Headers, Params, head).
try_url_sync(URL, Headers, Params, Method) -> try_url_sync(URL, Headers, Params, Method, ?TRIES).
try_url_sync(URL, ReqHeaders, Params, Method, N) ->
	case ibrowse:send_req(URL, ReqHeaders, Method, [], Params, infinity) of
		{ok, "404", _, _} -> not_found;
		{ok, _, _, _} when Method =:= head -> try_url_sync(URL, ReqHeaders, Params, get);
		{ok, Code, Headers, Body} ->
			Payload = case get_location(Headers) of
				no_location -> Body;
				Location ->
					case is_dir_redir(URL, Location) of
						true -> dir;
						false -> {redir, Location}
					end
			end,
			{Code, Payload};
		{error, retry_later} -> timer:sleep(100), try_url_sync(URL, ReqHeaders, Params, Method, N);
		{error, _} when N > 0 -> try_url_sync(URL, ReqHeaders, Params, Method, N - 1);
		{error, _} = Error -> Error
	end.


get_location([]) -> no_location;
get_location([{Header, Value} | Rest]) ->
	case string:to_lower(Header) of
		"location" -> Value;
		_ -> get_location(Rest)
	end.

is_dir_redir([C | URL], [C | Loc]) ->
	is_dir_redir(URL, Loc);
is_dir_redir("", "/") -> true;
is_dir_redir(_, _) -> false.

is_dir_redir_test() ->
	?assert(is_dir_redir("http://foo/bar", "http://foo/bar/")),
	?assertNot(is_dir_redir("http://foo/bar", "http://qux/baz/")).
