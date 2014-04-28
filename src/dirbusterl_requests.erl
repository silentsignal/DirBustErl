-module(dirbusterl_requests).
-export([increment/2, init_bust/0, remove_bust/0, start_link/0, stop/0, get_value/1]).
-export([dummy_process/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]). %% gen_server callbacks
-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("dirbusterl_requests_counter.erl").

%% External API

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

get_value(Pid) -> gen_server:call(?MODULE, {get_value, Pid}).
increment(_, 0) -> ok;
increment(Pos, Value) -> gen_server:cast(?MODULE, {increment, self(), Pos, Value}).
init_bust() -> gen_server:cast(?MODULE, {init_bust, self()}).
remove_bust() -> gen_server:cast(?MODULE, {remove_bust, self()}).

%% Callbacks for gen_server

init([]) -> {ok, ets:new(?MODULE, [{keypos, #requests.pid}])}.

handle_call(stop, _From, Tree) -> {stop, normal, ok, Tree};
handle_call({get_value, Pid}, _From, Tab) ->
	Result = case ets:lookup(Tab, Pid) of
		[R] -> [R#requests.issued, R#requests.all];
		[] -> null
	end,
	{reply, Result, Tab}.

handle_cast({remove_bust, Pid}, Tab) ->
	ets:delete(Tab, Pid),
	{noreply, Tab};
handle_cast({init_bust, Pid}, Tab) ->
	ets:insert(Tab, #requests{pid = Pid}),
	{noreply, Tab};
handle_cast({increment, Pid, Pos, Value}, Tab) ->
	ets:update_counter(Tab, Pid, {Pos, Value}),
	{noreply, Tab}.

terminate(_, Tab) -> ets:delete(Tab).

%% Unit tests

smoke_test() ->
	start_link(),
	init_bust(),
	?assertEqual(get_value(self()), [0, 0]),
	increment(#requests.issued, 42),
	?assertEqual(get_value(self()), [42, 0]),
	increment(#requests.all, 23),
	?assertEqual(get_value(self()), [42, 23]),
	stop().

dummy_process(Ref, Parent) ->
	Parent ! {Ref, self(), init_bust()},
	receive {Ref, Value} -> Parent ! {Ref, self(), increment(#requests.issued, Value)} end,
	receive {Ref, stop} -> Parent ! {Ref, self(), stopped} end.

multiprocess_test() ->
	{Pid, Ref} = spawn_monitor(?MODULE, start_link, []),
	receive {'DOWN', Ref, process, Pid, _} -> done end,
	TestRef = make_ref(),
	P1 = spawn(?MODULE, dummy_process, [TestRef, self()]),
	receive {TestRef, P1, _} -> ?assertEqual(get_value(P1), [0, 0]) end,
	P2 = spawn(?MODULE, dummy_process, [TestRef, self()]),
	receive {TestRef, P2, _} -> ?assertEqual(get_value(P2), [0, 0]) end,
	P1 ! {TestRef, 42},
	receive {TestRef, P1, _} -> ok end,
	?assertEqual(get_value(P1), [42, 0]),
	?assertEqual(get_value(P2), [0, 0]),
	P2 ! {TestRef, 23},
	receive {TestRef, P2, _} -> ok end,
	?assertEqual(get_value(P1), [42, 0]),
	?assertEqual(get_value(P2), [23, 0]),
	P1 ! {TestRef, stop},
	receive {TestRef, P1, stopped} -> ?assertEqual(get_value(P2), [23, 0]) end,
	P2 ! {TestRef, stop},
	stop().
