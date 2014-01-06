-module(dirbusterl_url_mangler).

-behavior(gen_event).
-export([init/1, handle_event/2, terminate/2]).

init(State) ->
	{ok, State}.

handle_event({finished, URL, Code, Contents}, {Rules, Server} = State)
		when Code =/= error, is_list(Contents) ->
	mangle_found(Rules, URL, Server),
	{ok, State};
handle_event(_, State) ->
	{ok, State}.

mangle_found([], _, _) -> done;
mangle_found([Rule | Rest], URL, Server) ->
	File = re:replace(URL, "/([^/]+)$", "/" ++ Rule, [{return, list}]),
	dirbusterl:bust_file(Server, File),
	mangle_found(Rest, URL, Server).

terminate(_Args, _State) ->
	ok.
