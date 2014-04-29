-module(waiter).
-export([start_link/2, worker_finished/1, worker_finished/4, worker_started/1, get_nprocs/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]). %% gen_server callbacks
-export([dispatch_event/3]). %% for spawning internal processes
-behavior(gen_server).
-record(state, {server, config, bust_id, event_mgr, nprocs=1}).

-include_lib("dirbusterl_requests_counter.erl").

-define(ENABLED(X), proplists:get_bool(X, Config)).
-define(CONFIG_MOD_MAP, [
	{dirbusterl_dir_follower, follow_dirs},
	{dirbusterl_redir_follower, follow_redirs},
	{dirbusterl_body_parser, parse_body}]).


%% External API

start_link(Config, BustId) ->
	{ok, EventMgr} = gen_event:start_link(),
	ok = gen_event:add_handler(EventMgr, dirbusterl_url_collector, BustId),
	[ok = gen_event:add_handler(EventMgr, M, self()) || {M, K} <- ?CONFIG_MOD_MAP, ?ENABLED(K)],
	ok = case proplists:get_value(mangle_found, Config, []) of
		[] -> ok;
		Rules -> gen_event:add_handler(EventMgr, dirbusterl_url_mangler, {Rules, self()})
	end,
	State = #state{server=self(), config=Config, bust_id=BustId, event_mgr=EventMgr},
	{ok, Pid} = gen_server:start_link(?MODULE, State, []),
	Pid.

worker_finished(Waiter) -> gen_server:cast(Waiter, finished).
worker_finished(Waiter, URL, Code, Contents) ->
	gen_server:cast(Waiter, {finished, URL, Code, Contents}).
worker_started(Waiter) -> gen_server:cast(Waiter, started).
get_nprocs(Waiter) -> gen_server:call(Waiter, get_nprocs).


%% Callbacks for gen_server

init(Args) -> {ok, Args}.

handle_cast(finished, S = #state{nprocs=1}) ->
	increase_finished(S),
	{stop, normal, S};
handle_cast(finished, S) ->
	increase_finished(S),
	{noreply, S#state{nprocs=S#state.nprocs - 1}};
handle_cast(started, S) -> {noreply, S#state{nprocs=S#state.nprocs + 1}};
handle_cast({finished, _, _, _} = Event, State) ->
	increase_finished(State),
	spawn_link(?MODULE, dispatch_event, [Event, State#state.event_mgr, self()]),
	{noreply, State}. %% LOCK

handle_call(get_nprocs, _From, S) -> {reply, S#state.nprocs, S}.

terminate(normal, S) -> S#state.server ! done.


%% Internal functions

increase_finished(#state{server = Server}) ->
	dirbusterl_requests:increment(Server, #requests.finished, 1).

dispatch_event(Event, Manager, Waiter) ->
	ok = gen_event:sync_notify(Manager, Event),
	worker_finished(Waiter). %% UNLOCK
