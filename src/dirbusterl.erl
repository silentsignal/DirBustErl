-module(dirbusterl).
-export([bust/2, bust_file/2, bust_dir/2, filter_burst_config/1]).
-include_lib("dirbusterl_server_state.hrl").

%% External API

bust(URL, UserConfig) ->
	URLs = ets:new(dirbusterl_urls, []),
	{Inputs, Config} = process_inputs_open(process_url_restriction(UserConfig)),
	Waiter = waiter:start_link(Config),
	process_url_lists(Inputs, Waiter, filter_burst_config(Config)),
	WordList = proplists:get_value(wordlist, Inputs),
	dirbusterl_server:bust(URL, dir, #state{waiter=Waiter, wordlist=WordList, config=Config, urls=URLs}),
	process_inputs_close(Inputs),
	ets:delete(URLs).

bust_file(Server, File) -> Server ! {bust_file, File}.
bust_dir(Server, Dir) -> Server ! {bust_dir, Dir}.

%% Internal functions

process_url_lists([], _, _) -> ok;
process_url_lists([{url_list, URLList} | Inputs], Waiter, Config) ->
	dirbusterl_server:burst_wordlist(url_list, #state{waiter=Waiter, wordlist=URLList, config=Config}),
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

process_url_restriction([{url_restriction, Restriction} | Config]) ->
	Value = case Restriction of
				X when is_list(X) ->
					{ok, RE} = re:compile(X),
					fun(URL) -> re:run(URL, RE, [{capture, none}]) =:= match end
			end,
	[{url_restriction, Value} | Config];
process_url_restriction([Item | Config]) -> [Item | process_url_restriction(Config)];
process_url_restriction([]) -> [].
