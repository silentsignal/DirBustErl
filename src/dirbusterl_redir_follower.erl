-module(dirbusterl_redir_follower).

-behavior(gen_event).
-export([init/1, handle_event/2, terminate/2]).

init(Server) ->
	{ok, Server}.

handle_event({finished, URL, _Code, {redir, Target}}, Server) ->
	dirbusterl:bust_file(Server, {URL, Target}),
	{ok, Server};
handle_event(_, Server) ->
	{ok, Server}.

terminate(_Args, _State) ->
	ok.
