-module(dirbusterl_requests).
-export([increment/1, init_bust/0, remove_bust/0, start_link/0, stop/0, get_value/1]).
-export([dummy_process/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]). %% gen_server callbacks
-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% External API

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

get_value(Pid) -> gen_server:call(?MODULE, {get_value, Pid}).
increment(Value) -> gen_server:cast(?MODULE, {increment, self(), Value}).
init_bust() -> gen_server:cast(?MODULE, {init_bust, self()}).
remove_bust() -> gen_server:cast(?MODULE, {remove_bust, self()}).

%% Callbacks for gen_server

init([]) -> {ok, ets:new(?MODULE, [])}.

handle_call(stop, _From, Tree) -> {stop, normal, ok, Tree};
handle_call({get_value, Pid}, _From, Tab) ->
	Result = case ets:lookup(Tab, Pid) of
		[{Pid, Counter}] -> Counter;
		[] -> null
	end,
	{reply, Result, Tab}.

handle_cast({remove_bust, Pid}, Tab) ->
	ets:delete(Tab, Pid),
	{noreply, Tab};
handle_cast({init_bust, Pid}, Tab) ->
	ets:insert(Tab, {Pid, 0}),
	{noreply, Tab};
handle_cast({increment, Pid, Value}, Tab) ->
	ets:update_counter(Tab, Pid, Value),
	{noreply, Tab}.

terminate(_, Tab) -> ets:delete(Tab).

%% Unit tests

smoke_test() ->
	start_link(),
	init_bust(),
	?assertEqual(get_value(self()), 0),
	increment(42),
	?assertEqual(get_value(self()), 42),
	stop().

dummy_process(Ref, Parent) ->
	Parent ! {Ref, self(), init_bust()},
	receive {Ref, Value} -> Parent ! {Ref, self(), increment(Value)} end,
	receive {Ref, stop} -> Parent ! {Ref, self(), stopped} end.

multiprocess_test() ->
	{Pid, Ref} = spawn_monitor(?MODULE, start_link, []),
	receive {'DOWN', Ref, process, Pid, _} -> done end,
	TestRef = make_ref(),
	P1 = spawn(?MODULE, dummy_process, [TestRef, self()]),
	receive {TestRef, P1, _} -> ?assertEqual(get_value(P1), 0) end,
	P2 = spawn(?MODULE, dummy_process, [TestRef, self()]),
	receive {TestRef, P2, _} -> ?assertEqual(get_value(P2), 0) end,
	P1 ! {TestRef, 42},
	receive {TestRef, P1, _} -> ok end,
	?assertEqual(get_value(P1), 42),
	?assertEqual(get_value(P2), 0),
	P2 ! {TestRef, 23},
	receive {TestRef, P2, _} -> ok end,
	?assertEqual(get_value(P1), 42),
	?assertEqual(get_value(P2), 23),
	P1 ! {TestRef, stop},
	receive {TestRef, P1, stopped} -> ?assertEqual(get_value(P2), 23) end,
	P2 ! {TestRef, stop},
	stop().
