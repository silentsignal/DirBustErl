-module(dirbusterl).
-compile([export_all]).

-define(WORDLIST, "/home/dnet/_app/DirBuster-0.12/directory-list-2.3-small.txt").
-define(TRIES, 8).
-define(BACKOFF_LIMIT, 16384).
-define(BACKOFF_INTERVAL, 64).
-define(BACKOFF_MSEC, 3000).
-define(ENABLED(X), lists:member(X, Config)).

bust(URL) -> bust(URL, []).
bust(URL, Config) ->
	Waiter = spawn_link(?MODULE, waiter, [self(), Config]),
	{ok, WordList} = file:open(?WORDLIST, [read, raw, read_ahead, binary]),
	bust(URL, Waiter, WordList, Config),
	file:close(WordList).

bust(URL, Waiter, WordList, Config) ->
	BaseURL = ensure_ends_with_slash(URL),
	spawn_worker(BaseURL, Waiter),
	file:position(WordList, bof),
	burst_wordlist(BaseURL, WordList, Waiter),
	Waiter ! finished,
	receive
		{bust, Target} -> bust(Target, Waiter, WordList, Config);
		done -> done
	end.

waiter(Server, Config) -> waiter(Server, Config, 1).
waiter(Server, _, 0) -> Server ! done;
waiter(Server, Config, NProcs) ->
	NewProcs = receive
		started -> NProcs + 1;
		finished -> NProcs - 1;
		{get, Pid} -> Pid ! {nprocs, NProcs}, NProcs;
		{finished, URL, Code, Contents} ->
			io:format("~s ~s\n", [Code, URL]),
			case {?ENABLED(follow_dirs), ?ENABLED(follow_redirs), Contents} of
				{true, _, dir} -> Server ! {bust, URL};
				{_, true, {redir, Target}} -> Server ! {bust, Target};
				_ -> ok
			end,
			NProcs - 1
	end,
	waiter(Server, Config, NewProcs).

ensure_ends_with_slash(Str) ->
	case lists:reverse(Str) of
		 [$/ | _] -> Str;
		 WithoutSlash -> lists:reverse([$/ | WithoutSlash])
	end.

burst_wordlist(BaseURL, WordList, Waiter) ->
	burst_wordlist(BaseURL, WordList, Waiter, ?BACKOFF_INTERVAL).
burst_wordlist(BaseURL, WordList, Waiter, 0) ->
	Waiter ! {get, self()},
	receive
		{nprocs, N} when N > ?BACKOFF_LIMIT ->
			timer:sleep(?BACKOFF_MSEC),
			burst_wordlist(BaseURL, WordList, Waiter, 0);
		{nprocs, _} -> burst_wordlist(BaseURL, WordList, Waiter)
	end;
burst_wordlist(BaseURL, WordList, Waiter, Check) ->
	case file:read_line(WordList) of
		{ok, <<$#, _/binary>>} -> burst_wordlist(BaseURL, WordList, Waiter, Check - 1);
		{ok, <<$\n>>} -> burst_wordlist(BaseURL, WordList, Waiter, Check - 1);
		{ok, Line} ->
			Postfix = binary_to_list(binary:replace(Line, <<$\n>>, <<>>)),
			spawn_worker(BaseURL ++ ibrowse_lib:url_encode(Postfix), Waiter),
			burst_wordlist(BaseURL, WordList, Waiter, Check - 1);
		eof -> ok
	end.

spawn_worker(URL, Waiter) ->
	Waiter ! started,
	spawn_link(dirbusterl, try_url, [URL, Waiter]).

try_url(URL, Waiter) -> try_url(URL, Waiter, ?TRIES).
try_url(URL, Waiter, N) ->
	case ibrowse:send_req(URL, [], get, [], [], infinity) of
		{ok, "404", _, _} -> Waiter ! finished;
		{ok, Code = [$3 | _], Headers, Body} ->
			Location = get_location(Headers),
			Payload = case get_location(Headers) of
				no_location -> Body;
				Location ->
					case is_dir_redir(URL, Location) of
						true -> dir;
						false -> {redir, get_location(Headers)}
					end
			end,
			Waiter ! {finished, URL, Code, Payload};
		{ok, Code, _, Body} -> Waiter ! {finished, URL, Code, Body};
		{error, retry_later} -> timer:sleep(100), try_url(URL, Waiter, N);
		{error, _} when N > 0 -> try_url(URL, Waiter, N - 1);
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
