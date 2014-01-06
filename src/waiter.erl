-module(waiter).
-export([start_link/2, worker_finished/1, worker_finished/4, worker_started/1, get_nprocs/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]). %% gen_server callbacks
-export([parse_body/4, found_file/5]). %% for spawning internal processes
-behavior(gen_server).
-record(state, {server, config, bust_id, event_mgr, nprocs=1}).

-include_lib("eunit/include/eunit.hrl").

-define(ENABLED(X), proplists:get_bool(X, Config)).


%% External API

start_link(Config, BustId) ->
	{ok, EventMgr} = gen_event:start_link(),
	ok = gen_event:add_handler(EventMgr, dirbusterl_url_collector, BustId),
	[ok = gen_event:add_handler(EventMgr,
		list_to_atom("dirbusterl_" ++ Key ++ "_follower"), self())
		|| Key <- ["dir", "redir"], ?ENABLED(list_to_atom("follow_" ++ Key ++ "s"))],
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

handle_cast(finished, S = #state{nprocs=1}) -> {stop, normal, S};
handle_cast(finished, S) -> {noreply, S#state{nprocs=S#state.nprocs - 1}};
handle_cast(started, S) -> {noreply, S#state{nprocs=S#state.nprocs + 1}};
handle_cast({finished, URL, Code, Contents} = Event, State) ->
	gen_event:notify(State#state.event_mgr, Event),
	NewState = process_url_found(URL, Code, Contents, State),
	handle_cast(finished, NewState).

handle_call(get_nprocs, _From, S) -> {reply, S#state.nprocs, S}.

terminate(normal, S) -> S#state.server ! done.


%% Internal functions

process_url_found(URL, Code, Contents, S) when Code =/= error, is_list(Contents) ->
	spawn_link(?MODULE, found_file, [Contents, URL, S#state.server, S#state.config, self()]),
	S#state{nprocs=S#state.nprocs + 1};
process_url_found(_, _, _, S) -> S.

found_file(Body, URL, Server, Config, Waiter) ->
	case ?ENABLED(parse_body) of
		true ->
			worker_started(Waiter),
			spawn_link(?MODULE, parse_body, [Body, URL, Server, Waiter]);
		false -> nop
	end,
	mangle_found(proplists:get_value(mangle_found, Config, []), URL, Server),
	worker_finished(Waiter).

mangle_found([], _, _) -> done;
mangle_found([Rule | Rest], URL, Server) ->
	File = re:replace(URL, "/([^/]+)$", "/" ++ Rule, [{return, list}]),
	dirbusterl:bust_file(Server, File),
	mangle_found(Rest, URL, Server).

parse_body(Body, URL, Server, Waiter) ->
	parse_body_values(extract_paths_from_body(Body), URL, Server),
	worker_finished(Waiter).

-define(BODY_RE_HTML_ATTRIBS, "(?:src|href|action)=(?:\"([^\"]+)\"|'([^']+)'|([^ >]+)[ >])").
-define(BODY_RE_ROBOTS_TXT, "(?:(?:dis)?allow|sitemap): (.*)\\n").
-define(BODY_RE_CSS_URL, "url\\(['\"]?([^'\")]+)['\"]?\\)").

extract_paths_from_body(Body) ->
	case re:run(Body, "(?:" ?BODY_RE_HTML_ATTRIBS "|" ?BODY_RE_ROBOTS_TXT "|" ?BODY_RE_CSS_URL ")",
		   [global, {capture, all, list}, caseless]) of
		{match, Results} -> lists:map(fun lists:last/1, Results);
		nomatch -> []
	end.

parse_body_values([], _, _) -> ok;
parse_body_values([Result | Rest], URL, Server) ->
	Value = string:sub_word(string:sub_word(Result, 1, $?), 1, $#), %% remove ?... #...
	dirbusterl:bust_file(Server, {URL, Value}),
	parse_body_values(Rest, URL, Server).

extract_paths_from_body_test() ->
	?assertEqual(
	   extract_paths_from_body("<img src='foo.png'>\nDisallow: /foo/bar\n"
										  "nameg\n\n@import url('/css/styles.css');\n"),
	   ["foo.png","/foo/bar","/css/styles.css"]
	  ).
