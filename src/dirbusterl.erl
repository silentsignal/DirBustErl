-module(dirbusterl).
-export([bust/2, bust_file/2, bust_dir/2]).
-include_lib("dirbusterl_server_state.hrl").


%% External API

bust(URL, UserConfig) ->
	URLs = ets:new(dirbusterl_urls, []),
	{Inputs, Config} = dirbusterl_config:process_user_config(UserConfig),
	Waiter = waiter:start_link(Config),
	process_url_lists(Inputs, Waiter, dirbusterl_config:filter_burst_config(Config)),
	WordList = proplists:get_value(wordlist, Inputs),
	dirbusterl_server:bust(URL, dir, #state{waiter=Waiter, wordlist=WordList, config=Config, urls=URLs}),
	dirbusterl_config:process_inputs_close(Inputs),
	ets:delete(URLs).

process_url_lists([], _, _) -> ok;
process_url_lists([{url_list, URLList} | Inputs], Waiter, Config) ->
	dirbusterl_server:burst_wordlist(url_list, #state{waiter=Waiter, wordlist=URLList, config=Config}),
	process_url_lists(Inputs, Waiter, Config);
process_url_lists([_ | Inputs], Waiter, Config) ->
	process_url_lists(Inputs, Waiter, Config).

bust_file(Server, File) -> Server ! {bust_file, File}.
bust_dir(Server, Dir) -> Server ! {bust_dir, Dir}.

