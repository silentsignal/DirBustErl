-module(dirbusterl).
-export([bust/1, bust/2]).

-define(BACKOFF_LIMIT, 16384).
-define(BACKOFF_INTERVAL, 64).
-define(BACKOFF_MSEC, 3000).

bust(URL) -> bust(URL, []).
bust(URL, UserConfig) ->
	URLs = ets:new(dirbusterl_urls, [named_table]),
	{Inputs, Config} = process_inputs_open(process_url_restriction(UserConfig)),
	Waiter = waiter:start_link(Config),
	process_url_lists(Inputs, Waiter, filter_burst_config(Config)),
	WordList = proplists:get_value(wordlist, Inputs),
	bust(URL, dir, Waiter, WordList, Config),
	process_inputs_close(Inputs),
	ets:delete(URLs).

bust(URL, Mode, Waiter, WordList, Config) ->
	spawn_worker(URL, Waiter, proplists:get_value(http_cfg, Config, [])),
	case Mode of
		dir ->
			file:position(WordList, bof),
			BaseURL = url_tools:ensure_ends_with_slash(URL),
			burst_wordlist(BaseURL, WordList, Waiter, filter_burst_config(Config));
		_ -> ok
	end,
	waiter:worker_finished(Waiter),
	server_loop(Waiter, WordList, Config).

process_url_lists([], _, _) -> ok;
process_url_lists([{url_list, URLList} | Inputs], Waiter, Config) ->
	burst_wordlist(url_list, URLList, Waiter, Config),
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

filter_burst_config(Config) -> filter_burst_config(Config, []).
filter_burst_config([], Acc) -> Acc;
filter_burst_config([Item | Config], Acc)
  when element(1, Item) =:= postfix; element(1, Item) =:= http_cfg ->
	filter_burst_config(Config, [Item | Acc]);
filter_burst_config([_ | Config], Acc) ->
	filter_burst_config(Config, Acc).

server_loop(Waiter, WordList, Config) ->
	receive
		{bust_file, {Base, Path}} ->
			BareBase = lists:reverse(url_tools:subslashes(lists:reverse(Base))),
			try_bust(url_tools:urljoin(BareBase, Path), file, Waiter, WordList, Config);
		{bust_file, Target} ->
			try_bust(Target, file, Waiter, WordList, Config);
		{bust_dir, Target} ->
			try_bust(Target, dir, Waiter, WordList, Config);
		done -> done
	end.

try_bust(URL, Mode, Waiter, WordList, Config) ->
	case url_allowed(URL, Config) of
		true ->
			waiter:worker_started(Waiter),
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

burst_wordlist(BaseURL, WordList, Waiter, Config) ->
	burst_wordlist(BaseURL, WordList, Waiter, Config, ?BACKOFF_INTERVAL).
burst_wordlist(BaseURL, WordList, Waiter, Config, 0) ->
	case waiter:get_nprocs(Waiter) of
		N when N > ?BACKOFF_LIMIT ->
			timer:sleep(?BACKOFF_MSEC),
			burst_wordlist(BaseURL, WordList, Waiter, Config, 0);
		_ -> burst_wordlist(BaseURL, WordList, Waiter, Config)
	end;
burst_wordlist(BaseURL, WordList, Waiter, Config, Check) ->
	NewCheck = case file:read_line(WordList) of
		{ok, <<$#, _/binary>>} -> Check;
		{ok, <<$\n>>} -> Check;
		{ok, Line} ->
			Postfix = binary_to_list(Line, 1, byte_size(Line) - 1),
			Params = proplists:get_value(http_cfg, Config, []),
			case BaseURL of
				url_list -> spawn_worker(Postfix, Waiter, Params);
				_ ->
					UserPF = ["" | proplists:get_value(postfix, Config, [])],
					[spawn_worker(
					   BaseURL ++ ibrowse_lib:url_encode(Postfix) ++ PF,
					   Waiter, Params) || PF <- UserPF]
			end,
			Check - 1;
		eof -> ok
	end,
	case NewCheck of
		ok -> ok;
		_ -> burst_wordlist(BaseURL, WordList, Waiter, Config, NewCheck)
	end.

spawn_worker(URL, Waiter, Params) ->
	case ets:insert_new(dirbusterl_urls, {URL}) of
		true ->
			waiter:worker_started(Waiter),
			spawn_link(worker, try_url, [URL, Waiter, Params]);
		false -> already_requested
	end.
