-module(waiter).
-export([start_link/1, worker_finished/1, worker_finished/4, worker_started/1, get_nprocs/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]). %% gen_server callbacks
-export([parse_body/3, found_file/4]). %% for spawning internal processes
-behavior(gen_server).
-record(state, {server, config, nprocs=1}).

-include_lib("eunit/include/eunit.hrl").

-define(ENABLED(X), proplists:get_bool(X, Config)).

%% External API

start_link(Config) ->
	State = #state{server=self(), config=Config},
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
handle_cast({finished, URL, Code, Contents}, State) ->
	print_url_found(URL, Code, Contents),
	NewState = process_url_found(URL, Code, Contents, State),
	handle_cast(finished, NewState).

handle_call(get_nprocs, _From, S) -> {reply, S#state.nprocs, S}.

terminate(normal, S) -> S#state.server ! done.

%% Internal functions

print_url_found(URL, Code, Contents) ->
	Spec = case Contents of
			   dir -> " [DIR]";
			   {redir, To} -> " -> " ++ To;
			   _ -> ""
		   end,
	io:format("~s ~s~s\n", [Code, URL, Spec]).

process_url_found(URL, Code, Contents, S) ->
	Config = S#state.config,
	case {?ENABLED(follow_dirs), ?ENABLED(follow_redirs), Contents} of
		{true, _, dir} -> S#state.server ! {bust_dir, URL ++ "/"};
		{_, true, {redir, Target}} -> S#state.server ! {bust_file, {URL, Target}};
		{_, _, Body} when Code =/= error, is_list(Body) ->
			spawn_link(?MODULE, found_file, [Body, URL, S#state.server, Config]);
		_ -> ok
	end,
	S.

found_file(Body, URL, Server, Config) ->
	case ?ENABLED(parse_body) of
		true -> spawn_link(?MODULE, parse_body, [Body, URL, Server]);
		false -> nop
	end,
	mangle_found(proplists:get_value(mangle_found, Config, []), URL, Server).

mangle_found([], _, _) -> done;
mangle_found([Rule | Rest], URL, Server) ->
	Server ! {bust_file, re:replace(URL, "/([^/]+)$", "/" ++ Rule, [{return, list}])},
	mangle_found(Rest, URL, Server).

parse_body(Body, URL, Server) ->
	parse_body_values(extract_paths_from_body(Body), URL, Server).

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
	Server ! {bust_file, {URL, Value}},
	parse_body_values(Rest, URL, Server).

extract_paths_from_body_test() ->
	?assertEqual(
	   extract_paths_from_body("<img src='foo.png'>\nDisallow: /foo/bar\n"
										  "nameg\n\n@import url('/css/styles.css');\n"),
	   ["foo.png","/foo/bar","/css/styles.css"]
	  ).
