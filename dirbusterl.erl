-module(dirbusterl).
-compile([export_all]).

-define(WORDLIST, "/home/dnet/_app/DirBuster-0.12/directory-list-2.3-small.txt").
-define(TRIES, 4).

bust(URL) ->
	{ok, WordList} = file:open(?WORDLIST, [read, raw, read_ahead, binary]),
	%%process_flag(trap_exit, true),
	BaseURL = ensure_ends_with_slash(URL),
	Waiter = spawn_link(?MODULE, waiter, [self()]),
	spawn_worker(BaseURL, Waiter),
	burst_wordlist(BaseURL, WordList, Waiter),
	file:close(WordList),
	Waiter ! finished,
	receive done -> done end.

waiter(Server) -> waiter(Server, 2).
waiter(Server, 0) -> Server ! done;
waiter(Server, NProcs) ->
	receive
		started -> waiter(Server, NProcs + 1);
		finished -> waiter(Server, NProcs - 1)
	end.

ensure_ends_with_slash(Str) ->
	case lists:reverse(Str) of
		 [$/ | _] -> Str;
		 WithoutSlash -> lists:reverse([$/ | WithoutSlash])
	end.

burst_wordlist(BaseURL, WordList, Waiter) ->
	case file:read_line(WordList) of
		{ok, <<$#, _/binary>>} -> burst_wordlist(BaseURL, WordList, Waiter);
		{ok, <<$\n>>} -> burst_wordlist(BaseURL, WordList, Waiter);
		{ok, Line} ->
			Postfix = binary_to_list(binary:replace(Line, <<$\n>>, <<>>)),
			Waiter ! started,
			spawn_worker(BaseURL ++ ibrowse_lib:url_encode(Postfix), Waiter),
			burst_wordlist(BaseURL, WordList, Waiter);
		eof -> ok
	end.

spawn_worker(URL, Waiter) ->
	spawn_link(dirbusterl, try_url, [URL, Waiter]).

try_url(URL, Waiter) -> try_url(URL, Waiter, ?TRIES).
try_url(URL, Waiter, N) ->
	case ibrowse:send_req(URL, [], get, [], [], infinity) of
		{ok, "404", _, _} -> Waiter ! finished;
		{ok, Code = [$3 | _], Headers, _} ->
			Location = get_location(Headers),
			case get_location(Headers) of
				no_location -> io:format("~s ~s\n", [Code, URL]);
				Location ->
					case is_dir_redir(URL, Location) of
						true -> io:format("~s ~s [DIR]\n", [Code, URL]);
						false -> io:format("~s ~s -> ~s\n", [Code, URL, get_location(Headers)])
					end
			end,
			Waiter ! finished;
		{ok, Code, _, _} -> io:format("~s ~s\n", [Code, URL]), Waiter ! finished;
		{error, retry_later} -> timer:sleep(100), try_url(URL, Waiter, N);
		{error, _} when N > 0 -> try_url(URL, Waiter, N - 1)
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
