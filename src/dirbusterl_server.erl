-module(dirbusterl_server).
-export([bust/3, burst_wordlist/2]).
-include_lib("dirbusterl_server_state.hrl").

-define(BACKOFF_LIMIT, 16384).
-define(BACKOFF_INTERVAL, 64).
-define(BACKOFF_MSEC, 3000).

-define(getStateConfigList(X), proplists:get_value(X, State#state.config, [])).

bust(URL, Mode, State) ->
	spawn_worker(URL, State, ?getStateConfigList(http_cfg)),
	case Mode of
		dir ->
			file:position(State#state.wordlist, bof),
			BaseURL = url_tools:ensure_ends_with_slash(URL),
			burst_wordlist(BaseURL,
				State#state{config=dirbusterl:filter_burst_config(State#state.config)});
		_ -> ok
	end,
	case url_tools:has_at_least_n_slashes(URL, 4) of
		true ->
			[$/ | File] = Dir = url_tools:subslashes(tl(lists:reverse(URL))),
			dirbusterl:bust_file(self(), lists:reverse(File)),
			dirbusterl:bust_dir(self(), lists:reverse(Dir));
		false -> nop
	end,
	server_loop(State).

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
			bust(URL, Mode, S);
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