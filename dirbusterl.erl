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
	bust(URL, dir, Waiter, WordList, process_url_restriction(Config)),
	file:close(WordList),
	ets:delete(URLs).

bust(URL, Mode, Waiter, WordList, Config) ->
	spawn_worker(URL, Waiter),
	case Mode of
		dir ->
			file:position(WordList, bof),
			BaseURL = ensure_ends_with_slash(URL),
			burst_wordlist(BaseURL, WordList, Waiter, filter_burst_config(Config));
		_ -> ok
	end,
	Waiter ! finished,
	server_loop(Waiter, WordList, Config).

filter_burst_config(Config) -> filter_burst_config(Config, []).
filter_burst_config([], Acc) -> Acc;
filter_burst_config([Item | Config], Acc) when element(1, Item) =:= postfix ->
	filter_burst_config(Config, [Item | Acc]);
filter_burst_config([_ | Config], Acc) ->
	filter_burst_config(Config, Acc).

server_loop(Waiter, WordList, Config) ->
	receive
		{bust_file, {Base, Path}} ->
			BareBase = lists:reverse(subslashes(lists:reverse(Base))),
			try_bust(urljoin(BareBase, Path), file, Waiter, WordList, Config);
		{bust_dir, Target} ->
			try_bust(Target, dir, Waiter, WordList, Config);
		done -> done
	end.

try_bust(URL, Mode, Waiter, WordList, Config) ->
	case url_allowed(URL, Config) of
		true ->
			Waiter ! started,
			bust(URL, Mode, Waiter, WordList, Config);
		false ->
			server_loop(Waiter, WordList, Config)
	end.

url_allowed(URL, Config) ->
	case proplists:get_value(url_restriction, Config) of
		undefined -> true;
		Restriction -> Restriction(URL)
	end.

process_url_restriction([{url_restriction, Restriction} | Config]) ->
	Value = case Restriction of
				X when is_list(X) ->
					{ok, RE} = re:compile(X),
					fun(URL) -> re:run(URL, RE, [{capture, none}]) =:= match end
			end,
	[{url_restriction, Value} | Config];
process_url_restriction([Item | Config]) -> [Item | process_url_restriction(Config)];
process_url_restriction([]) -> [].

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
				{true, _, _, dir} -> Server ! {bust_dir, URL ++ "/"};
				{_, true, _, {redir, Target}} -> Server ! {bust_file, {URL, Target}};
				{_, _, true, Body} when Code =/= error, Body =/= "", is_list(Body) ->
					spawn_link(?MODULE, parse_body, [Body, URL, Server]);
				_ -> ok
			end,
			NProcs - 1
	end,
	waiter(Server, Config, NewProcs).

parse_body(Body, URL, Server) ->
	parse_body_values(extract_paths_from_body(Body), URL, Server).

-define(BODY_RE_HTML_ATTRIBS, "(?:src|href|action)=(?:\"([^\"]+)\"|'([^']+)'|([^ >]+)[ >])").
-define(BODY_RE_ROBOTS_TXT, "(?:(?:dis)?allow|sitemap): (.*)\\n").

extract_paths_from_body(Body) ->
	case re:run(Body, "(?:" ?BODY_RE_HTML_ATTRIBS "|" ?BODY_RE_ROBOTS_TXT ")",
		   [global, {capture, all, list}, caseless]) of
		{match, Results} -> lists:map(fun lists:last/1, Results);
		nomatch -> []
	end.

parse_body_values([], _, _) -> ok;
parse_body_values([Result | Rest], URL, Server) ->
	Value = string:sub_word(string:sub_word(Result, 1, $?), 1, $#), %% remove ?... #...
	Server ! {bust_file, {URL, Value}},
	parse_body_values(Rest, URL, Server).

ensure_ends_with_slash(Str) ->
	case lists:last(Str) of
		$/ -> Str;
		_ -> Str ++ "/"
	end.

burst_wordlist(BaseURL, WordList, Waiter, Config) ->
	burst_wordlist(BaseURL, WordList, Waiter, Config, ?BACKOFF_INTERVAL).
burst_wordlist(BaseURL, WordList, Waiter, Config, 0) ->
	Waiter ! {get, self()},
	receive
		{nprocs, N} when N > ?BACKOFF_LIMIT ->
			timer:sleep(?BACKOFF_MSEC),
			burst_wordlist(BaseURL, WordList, Waiter, Config, 0);
		{nprocs, _} -> burst_wordlist(BaseURL, WordList, Waiter, Config)
	end;
burst_wordlist(BaseURL, WordList, Waiter, Config, Check) ->
	case file:read_line(WordList) of
		{ok, <<$#, _/binary>>} -> burst_wordlist(BaseURL, WordList, Waiter, Config, Check);
		{ok, <<$\n>>} -> burst_wordlist(BaseURL, WordList, Waiter, Config, Check);
		{ok, Line} ->
			Postfix = binary_to_list(Line, 1, byte_size(Line) - 1),
			UserPF = ["" | proplists:get_value(postfix, Config, [])],
			[spawn_worker(BaseURL ++ ibrowse_lib:url_encode(Postfix) ++ PF, Waiter) || PF <- UserPF],
			burst_wordlist(BaseURL, WordList, Waiter, Config, Check - 1);
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
						false -> {redir, Location}
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
