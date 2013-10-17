-module(dirbusterl).
-export([bust/2, bust_file/2, bust_dir/2]).
-record(state, {waiter, wordlist, config, urls}).

-define(BACKOFF_LIMIT, 16384).
-define(BACKOFF_INTERVAL, 64).
-define(BACKOFF_MSEC, 3000).

-define(getStateConfigList(X), proplists:get_value(X, State#state.config, [])).

%% External API

bust(URL, UserConfig) ->
	URLs = ets:new(dirbusterl_urls, []),
	{Inputs, Config} = process_inputs_open(process_url_restriction(UserConfig)),
	Waiter = waiter:start_link(Config),
	process_url_lists(Inputs, Waiter, filter_burst_config(Config)),
	WordList = proplists:get_value(wordlist, Inputs),
	bust(URL, dir, #state{waiter=Waiter, wordlist=WordList, config=Config, urls=URLs}),
	process_inputs_close(Inputs),
	ets:delete(URLs).

bust_file(Server, File) -> Server ! {bust_file, File}.
bust_dir(Server, Dir) -> Server ! {bust_dir, Dir}.

%% Internal functions

bust(URL, Mode, State) ->
	spawn_worker(URL, State, ?getStateConfigList(http_cfg)),
	case Mode of
		dir ->
			file:position(State#state.wordlist, bof),
			BaseURL = url_tools:ensure_ends_with_slash(URL),
			burst_wordlist(BaseURL,
				State#state{config=filter_burst_config(State#state.config)});
		_ -> ok
	end,
	case url_tools:has_at_least_n_slashes(URL, 4) of
		true ->
			[$/ | File] = Dir = url_tools:subslashes(tl(lists:reverse(URL))),
			bust_file(self(), File),
			bust_dir(self(), Dir);
		false -> nop
	end,
	server_loop(State).

process_url_lists([], _, _) -> ok;
process_url_lists([{url_list, URLList} | Inputs], Waiter, Config) ->
	burst_wordlist(url_list, #state{waiter=Waiter, wordlist=URLList, config=Config}),
	process_url_lists(Inputs, Waiter, Config);
process_url_lists([_ | Inputs], Waiter, Config) ->
	process_url_lists(Inputs, Waiter, Config).

process_inputs_open(Config) -> process_inputs_open(Config, [], []).
process_inputs_open([], InputsAcc, ConfigAcc) -> {InputsAcc, ConfigAcc};
process_inputs_open([{Type, FileName} | Config], InputsAcc, ConfigAcc)
  when Type =:= wordlist; Type =:= url_list ->
	{ok, FP} = file:open(FileName, [read, raw, read_ahead, binary]),
	process_inputs_open(Config, [{Type, FP} | InputsAcc], ConfigAcc);
process_inputs_open([Item | Config], InputsAcc, ConfigAcc) ->
	process_inputs_open(Config, InputsAcc, [Item | ConfigAcc]).

process_inputs_close([]) -> ok;
process_inputs_close([{_, FP} | Inputs]) ->
	file:close(FP),
	process_inputs_close(Inputs).

filter_burst_config(Config) ->
	[Item || Item <- Config,
	 element(1, Item) =:= postfix orelse element(1, Item) =:= http_cfg].

server_loop(State) ->
	receive
		{bust_file, {Base, Path}} ->
			BareBase = lists:reverse(url_tools:subslashes(lists:reverse(Base))),
			try_bust(url_tools:urljoin(BareBase, Path), file, State);
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
			bust(URL, Mode, S);
		false ->
			server_loop(S)
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
	NewCheck = case file:read_line(State#state.wordlist) of
		{ok, <<$#, _/binary>>} -> Check;
		{ok, <<$\n>>} -> Check;
		{ok, Line} ->
			Postfix = binary_to_list(Line, 1, byte_size(Line) - 1),
			Params = ?getStateConfigList(http_cfg),
			case BaseURL of
				url_list -> spawn_worker(Postfix, State, Params);
				_ ->
					UserPF = ["" | ?getStateConfigList(postfix)],
					[spawn_worker(
					   BaseURL ++ ibrowse_lib:url_encode(Postfix) ++ PF,
					   State, Params) || PF <- UserPF]
			end,
			Check - 1;
		eof -> ok
	end,
	case NewCheck of
		ok -> ok;
		_ -> burst_wordlist(BaseURL, State, NewCheck)
	end.

spawn_worker(URL, State, Params) ->
	case ets:insert_new(State#state.urls, {URL}) of
		true ->
			Waiter = State#state.waiter,
			waiter:worker_started(Waiter),
			spawn_link(worker, try_url, [URL, Waiter, Params]);
		false -> already_requested
	end.
