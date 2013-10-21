-module(dirbusterl_config).
-export([process_inputs_close/1, filter_burst_config/1, process_user_config/1]).

process_inputs_close([]) -> ok;
process_inputs_close([{_, FP} | Inputs]) ->
	file:close(FP),
	process_inputs_close(Inputs).

filter_burst_config(Config) ->
	[Item || Item <- Config,
	 element(1, Item) =:= postfix orelse element(1, Item) =:= http_cfg].

process_user_config(Config) -> process_user_config(Config, [], []).
process_user_config([], InputsAcc, ConfigAcc) -> {InputsAcc, ConfigAcc};

%% open input files (wordlists, URL lists)
process_user_config([{Type, FileName} | Config], InputsAcc, ConfigAcc)
  when Type =:= wordlist; Type =:= url_list ->
	{ok, FP} = file:open(FileName, [read, raw, read_ahead, binary]),
	process_user_config(Config, [{Type, FP} | InputsAcc], ConfigAcc);

%% compile URL restriction regular expressions
process_user_config([{url_restriction, Restriction} | Config], InputsAcc, ConfigAcc) ->
	Value = case Restriction of
				X when is_list(X) ->
					{ok, RE} = re:compile(X),
					fun(URL) -> re:run(URL, RE, [{capture, none}]) =:= match end
			end,
	process_user_config(Config, InputsAcc, [{url_restriction, Value} | ConfigAcc]);

%% leave everything unchanged
process_user_config([Item | Config], InputsAcc, ConfigAcc) ->
	process_user_config(Config, InputsAcc, [Item | ConfigAcc]).
