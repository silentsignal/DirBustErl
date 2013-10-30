-module(dirbusterl).
-export([start/0, start_link/0, stop/0]).
-export([bust/2, bust_async/3, bust_file/2, bust_dir/2]).
-export([bust_core/3, bust_monitor/3]). %% for spawning
-include_lib("dirbusterl_server_state.hrl").


%% External API

bust(URL, UserConfig) ->
	Id = dirbusterl_storage:generate_bust_id(),
	bust_async(Id, URL, UserConfig),
	bust_monitor(URL, UserConfig, Id),
	dirbusterl_storage:get_findings(Id).

bust_async(Id, URL, UserConfig) ->
	dirbusterl_storage:register_bust(Id, URL, UserConfig),
	spawn(?MODULE, bust_monitor, [URL, UserConfig, Id]).

bust_monitor(URL, UserConfig, Id) ->
	{Pid, Ref} = spawn_monitor(?MODULE, bust_core, [URL, UserConfig, Id]),
	dirbusterl_storage:set_server_pid(Id, Pid),
	receive
		{'DOWN', Ref, process, Pid, Info} ->
			dirbusterl_storage:set_server_pid(Id, {finished, Info})
	end.

bust_core(URL, UserConfig, Id) ->
	URLs = ets:new(dirbusterl_urls, []),
	{Inputs, Config} = dirbusterl_config:process_user_config(UserConfig),
	Waiter = waiter:start_link(Config, Id),
	process_url_lists(Inputs, Id, Waiter, dirbusterl_config:filter_burst_config(Config)),
	WordList = proplists:get_value(wordlist, Inputs),
	dirbusterl_server:bust(URL, dir,
		#state{waiter=Waiter, wordlist=WordList, config=Config, urls=URLs, id=Id}),
	dirbusterl_config:process_inputs_close(Inputs),
	ets:delete(URLs).

process_url_lists([], _, _, _) -> ok;
process_url_lists([{url_list, URLList} | Inputs], Id, Waiter, Config) ->
	dirbusterl_server:burst_wordlist(url_list,
		#state{waiter=Waiter, wordlist=URLList, config=Config, id=Id}),
	process_url_lists(Inputs, Id, Waiter, Config);
process_url_lists([_ | Inputs], Id, Waiter, Config) ->
	process_url_lists(Inputs, Id, Waiter, Config).

bust_file(Server, File) -> Server ! {bust_file, File}.
bust_dir(Server, Dir) -> Server ! {bust_dir, Dir}.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(ibrowse),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    dirbusterl_storage:init_schema(),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    dirbusterl_sup:start_link().

%% @spec start() -> ok
%% @doc Start the dirbusterl server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(ibrowse),
    mnesia:create_schema([node()]),
    ensure_started(mnesia),
    dirbusterl_storage:init_schema(),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(dirbusterl).

%% @spec stop() -> ok
%% @doc Stop the dirbusterl server.
stop() ->
    Res = application:stop(dirbusterl),
    application:stop(webmachine),
    application:stop(mnesia),
    application:stop(ibrowse),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
