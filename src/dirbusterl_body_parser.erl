-module(dirbusterl_body_parser).

-behavior(gen_event).
-export([init/1, handle_event/2, terminate/2]).

-include_lib("eunit/include/eunit.hrl").

init(Server) ->
	{ok, Server}.

handle_event({finished, URL, Code, Contents}, Server)
		when Code =/= error, is_list(Contents) ->
	parse_body_values(extract_paths_from_body(Contents), URL, Server),
	{ok, Server};
handle_event(_, Server) ->
	{ok, Server}.

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

terminate(_Args, _Server) ->
	ok.
