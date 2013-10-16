-module(worker).
-export([try_url/3]).

-define(TRIES, 8).

try_url(URL, Waiter, Params) -> try_url(URL, Waiter, Params, head).
try_url(URL, Waiter, Params, Method) -> try_url(URL, Waiter, Params, Method, ?TRIES).
try_url(URL, Waiter, Params, Method, N) ->
	case ibrowse:send_req(URL, [], Method, [], Params, infinity) of
		{ok, "404", _, _} -> Waiter ! finished;
		{ok, _, _, _} when Method =:= head -> try_url(URL, Waiter, Params, get);
		{ok, Code, Headers, Body} ->
			Payload = case get_location(Headers) of
				no_location -> Body;
				Location ->
					case is_dir_redir(URL, Location) of
						true -> dir;
						false -> {redir, Location}
					end
			end,
			Waiter ! {finished, URL, Code, Payload};
		{error, retry_later} -> timer:sleep(100), try_url(URL, Waiter, Params, Method, N);
		{error, _} when N > 0 -> try_url(URL, Waiter, Params, Method, N - 1);
		{error, Reason} -> Waiter ! {finished, URL, error, Reason}
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
