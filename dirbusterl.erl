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
	URLs = ets:new(dirbusterl_urls, [named_table]),
	Waiter = spawn_link(?MODULE, waiter, [self(), Config]),
	{ok, WordList} = file:open(?WORDLIST, [read, raw, read_ahead, binary]),
	bust(URL, Waiter, WordList, Config),
	file:close(WordList),
	ets:delete(URLs).

bust(URL, Waiter, WordList, Config) ->
	BaseURL = ensure_ends_with_slash(URL),
	spawn_worker(URL, Waiter),
	file:position(WordList, bof),
	burst_wordlist(BaseURL, WordList, Waiter),
	Waiter ! finished,
	receive
		{bust, {Base, Path}} -> bust(urljoin(ensure_ends_with_slash(Base), Path),
									Waiter, WordList, Config);
		{bust, Target} -> bust(Target, Waiter, WordList, Config);
		done -> done
	end.

urljoin(_, [$h, $t, $t, $p, $:, $/, $/ | _] = Path) -> Path;
urljoin(_, [$h, $t, $t, $p, $s, $:, $/, $/ | _] = Path) -> Path;
urljoin(Base, [$., $/ | Rest]) -> urljoin(Base, Rest);
urljoin(Base, [$., $., $/ | Rest]) ->
	urljoin(lists:reverse(subslashes(tl(lists:reverse(Base)))), Rest);
urljoin(Base, Path) -> Base ++ Path.

subslashes([$/ | _] = URL) -> URL;
subslashes([_ | Rest]) -> subslashes(Rest).

waiter(Server, Config) -> waiter(Server, Config, 1).
waiter(Server, _, 0) -> Server ! done;
waiter(Server, Config, NProcs) ->
	NewProcs = receive
		started -> NProcs + 1;
		finished -> NProcs - 1;
		{get, Pid} -> Pid ! {nprocs, NProcs}, NProcs;
		{finished, URL, Code, Contents} ->
			Spec = case Contents of
					   dir -> " [DIR]";
					   {redir, To} -> " -> " ++ To;
					   _ -> ""
				   end,
			io:format("~s ~s~s\n", [Code, URL, Spec]),
			case {?ENABLED(follow_dirs), ?ENABLED(follow_redirs), ?ENABLED(parse_body), Contents} of
				{true, _, _, dir} -> Server ! {bust, URL ++ "/"};
				{_, true, _, {redir, Target}} -> Server ! {bust, {URL, Target}};
				{_, _, true, Body} when Code =/= error, Body =/= "", is_list(Body) ->
					spawn_link(?MODULE, parse_body, [Body, URL, Server]);
				_ -> ok
			end,
			NProcs - 1
	end,
	waiter(Server, Config, NewProcs).

parse_body(Body, URL, Server) ->
	case re:run(Body, "(?:src|href|action)=(?:\"([^\"]+)\"|'([^']+)'|([^ >]+)[ >])",
		   [global, {capture, all, list}, caseless]) of
		{match, Results} -> parse_body_values(Results, URL, Server);
		nomatch -> ok
	end.

parse_body_values([], _, _) -> ok;
parse_body_values([Result | Rest], URL, Server) ->
	Value = string:sub_word(lists:last(Result), 1, $?), %% remove ?foo=bar&...
	Server ! {bust, {URL, Value}},
	parse_body_values(Rest, URL, Server).

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
	case ets:insert_new(dirbusterl_urls, {URL}) of
		true ->
			Waiter ! started,
			spawn_link(dirbusterl, try_url, [URL, Waiter]);
		false -> already_requested
	end.

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
