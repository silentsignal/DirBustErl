-module(dirbusterl).
-export([bust/2, bust_async/2, bust_file/2, bust_dir/2]).
-export([bust_core/3]). %% for spawning
-include_lib("dirbusterl_server_state.hrl").


%% External API

bust(URL, UserConfig) ->
	Id = bust_async(URL, UserConfig),
	Pid = dirbusterl_storage:get_server_pid(Id),
	Ref = monitor(process, Pid),
	receive
		{'DOWN', Ref, process, Pid, _} -> ok
	end,
	dirbusterl_storage:get_findings(Id).

bust_async(URL, UserConfig) ->
	Id = dirbusterl_storage:allocate_bust_id(URL, UserConfig),
	Pid = spawn(?MODULE, bust_core, [URL, UserConfig, Id]),
	dirbusterl_storage:set_server_pid(Id, Pid),
	Id.

bust_core(URL, UserConfig, Id) ->
	URLs = ets:new(dirbusterl_urls, []),
	{Inputs, Config} = dirbusterl_config:process_user_config(UserConfig),
	Waiter = waiter:start_link(Config, Id),
	process_url_lists(Inputs, Id, Waiter, dirbusterl_config:filter_burst_config(Config)),
	WordList = proplists:get_value(wordlist, Inputs),
	dirbusterl_server:bust(URL, dir,
		#state{waiter=Waiter, wordlist=WordList, config=Config, urls=URLs, id=Id}),
	dirbusterl_config:process_inputs_close(Inputs),
	ets:delete(URLs),
	dirbusterl_storage:set_server_pid(Id, finished).

process_url_lists([], _, _, _) -> ok;
process_url_lists([{url_list, URLList} | Inputs], Id, Waiter, Config) ->
	dirbusterl_server:burst_wordlist(url_list,
		#state{waiter=Waiter, wordlist=URLList, config=Config, id=Id}),
	process_url_lists(Inputs, Id, Waiter, Config);
process_url_lists([_ | Inputs], Id, Waiter, Config) ->
	process_url_lists(Inputs, Id, Waiter, Config).

bust_file(Server, File) -> Server ! {bust_file, File}.
bust_dir(Server, Dir) -> Server ! {bust_dir, Dir}.
