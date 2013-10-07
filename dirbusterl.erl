-module(dirbusterl).
-compile([export_all]).

-define(WORDLIST, "/home/dnet/_app/DirBuster-0.12/directory-list-2.3-small.txt").
-define(TRIES, 4).

bust(URL) ->
	{ok, WordList} = file:open(?WORDLIST, [read, raw, read_ahead, binary]),
	%%process_flag(trap_exit, true),
	BaseURL = ensure_ends_with_slash(URL),
	spawn_worker(BaseURL),
	NProcs = burst_wordlist(BaseURL, WordList, 1),
	io:format("~p\n", [NProcs]),
	wait_for_procs(NProcs),
	file:close(WordList).

wait_for_procs(0) -> ok;
wait_for_procs(NProcs) ->
	receive {'DOWN', _, _, _, _} -> ok end,
	wait_for_procs(NProcs - 1).

ensure_ends_with_slash(Str) ->
	case lists:reverse(Str) of
		 [$/ | _] -> Str;
		 WithoutSlash -> lists:reverse([$/ | WithoutSlash])
	end.

burst_wordlist(BaseURL, WordList, N) ->
	case file:read_line(WordList) of
		{ok, <<$#, _/binary>>} -> burst_wordlist(BaseURL, WordList, N);
		{ok, <<$\n>>} -> burst_wordlist(BaseURL, WordList, N);
		{ok, Line} ->
			Postfix = binary_to_list(binary:replace(Line, <<$\n>>, <<>>)),
			spawn_worker(BaseURL ++ ibrowse_lib:url_encode(Postfix)),
			burst_wordlist(BaseURL, WordList, N + 1);
		eof -> N
	end.

spawn_worker(URL) ->
	spawn_monitor(dirbusterl, try_url, [URL]).

try_url(URL) -> try_url(URL, ?TRIES).
try_url(URL, N) ->
	case ibrowse:send_req(URL, [], get, [], [], infinity) of
		{ok, "404", _, _} -> ok;
		{ok, Code = [$3 | _], Headers, _} ->
			Location = get_location(Headers),
			case get_location(Headers) of
				no_location -> io:format("~s ~s\n", [Code, URL]);
				Location ->
					case is_dir_redir(URL, Location) of
						true -> io:format("~s ~s [DIR]\n", [Code, URL]);
						false -> io:format("~s ~s -> ~s\n", [Code, URL, get_location(Headers)])
					end
			end;
		{ok, Code, _, _} -> io:format("~s ~s\n", [Code, URL]);
		{error, retry_later} -> timer:sleep(100), try_url(URL, N);
		{error, _} when N > 0 -> try_url(URL, N - 1)
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
