-module(dirbusterl_dir_follower).

-behavior(gen_event).
-export([init/1, handle_event/2, terminate/2]).

init(Server) ->
	{ok, Server}.

handle_event({finished, URL, _Code, dir}, Server) ->
	dirbusterl:bust_dir(Server, URL ++ "/"),
	{ok, Server};
handle_event(_, Server) ->
	{ok, Server}.

terminate(_Args, _State) ->
	ok.
