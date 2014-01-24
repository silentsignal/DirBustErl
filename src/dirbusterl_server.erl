-module(dirbusterl_server).
-export([bust/3, burst_wordlist/2]).
-include_lib("dirbusterl_server_state.hrl").

-define(BACKOFF_LIMIT, 16384).
-define(BACKOFF_INTERVAL, 64).
-define(BACKOFF_MSEC, 3000).

-define(getStateConfigList(X), proplists:get_value(X, State#state.config, [])).
-define(incrementReqs(S, V), (S)#state{issued_reqs=(S)#state.issued_reqs + (V)}).

bust(URL, Mode, State) ->
	NewReqs = spawn_worker(URL, State, ?getStateConfigList(http_cfg)) +
	case Mode of
		dir ->
			file:position(State#state.wordlist, bof),
			BaseURL = url_tools:ensure_ends_with_slash(URL),
			burst_wordlist(BaseURL,
				State#state{config=dirbusterl_config:filter_burst_config(State#state.config)});
		_ -> 0
	end,
	case url_tools:has_at_least_n_slashes(URL, 4) of
		true ->
			[$/ | File] = Dir = url_tools:subslashes(tl(lists:reverse(URL))),
			dirbusterl:bust_file(self(), lists:reverse(File)),
			dirbusterl:bust_dir(self(), lists:reverse(Dir));
		false -> nop
	end,
	server_loop(?incrementReqs(State, NewReqs)).

server_loop(State) ->
	receive
		{bust_file, {Base, Path}} ->
			try_bust(url_tools:urljoin(Base, Path), file, State);
		{bust_file, Target} ->
			try_bust(Target, file, State);
		{bust_dir, Target} ->
			try_bust(Target, dir, State)
		after 100 ->
			Waiter = State#state.waiter,
			case waiter:get_nprocs(Waiter) of
				1 -> waiter:worker_finished(Waiter), done;
				_ -> server_loop(State)
			end
	end.

try_bust(URL, Mode, S) ->
	case url_allowed(URL, S#state.config) of
		true ->
			case dirbusterl_visited_urls:book_visit(S#state.parents, URL) of
				true -> bust(URL, Mode, S);
				false -> server_loop(S)
			end;
		false ->
			server_loop(S)
	end.

url_allowed(URL, Config) ->
	case proplists:get_value(url_restriction, Config) of
		undefined -> true;
		Restriction -> Restriction(URL)
	end.

burst_wordlist(BaseURL, State) ->
	burst_wordlist(BaseURL, State, ?BACKOFF_INTERVAL).
burst_wordlist(BaseURL, State, 0) ->
	case waiter:get_nprocs(State#state.waiter) of
		N when N > ?BACKOFF_LIMIT ->
			timer:sleep(?BACKOFF_MSEC),
			burst_wordlist(BaseURL, State, 0);
		_ -> burst_wordlist(BaseURL, State)
	end;
burst_wordlist(BaseURL, State, Check) ->
	{NewReqs, NewCheck} = case file:read_line(State#state.wordlist) of
		{ok, <<$#, _/binary>>} -> {0, Check};
		{ok, <<$\n>>} -> {0, Check};
		{ok, Line} ->
			Postfix = binary_to_list(Line, 1, byte_size(Line) - 1),
			Params = ?getStateConfigList(http_cfg),
			NR = case BaseURL of
				url_list -> spawn_worker(Postfix, State, Params);
				_ ->
					UserPF = ["" | ?getStateConfigList(postfix)],
					ExtendedBase = BaseURL ++ ibrowse_lib:url_encode(Postfix),
					lists:sum([spawn_worker(ExtendedBase ++ PF, State, Params) || PF <- UserPF])
			end,
			{NR, Check - 1};
		eof -> {0, ok}
	end,
	case NewCheck of
		ok -> State#state.issued_reqs + NewReqs;
		_ ->
			burst_wordlist(BaseURL, ?incrementReqs(State, NewReqs), NewCheck)
	end.

spawn_worker(URL, State, Params) ->
	case dirbusterl_visited_urls:book_visit(State#state.urls, URL) of
		true ->
			Waiter = State#state.waiter,
			waiter:worker_started(Waiter),
			spawn_link(worker, try_url, [URL, Waiter, Params]),
			1;
		false -> 0
	end.
