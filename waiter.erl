-module(waiter).
-export([waiter/2, parse_body/3, found_file/4]).

-define(ENABLED(X), proplists:get_bool(X, Config)).

waiter(Server, Config) -> waiter(Server, Config, 1).
waiter(Server, _, 0) -> Server ! done;
waiter(Server, Config, NProcs) ->
	NewProcs = receive
		started -> NProcs + 1;
		finished -> NProcs - 1;
		{get, Pid} -> Pid ! {nprocs, NProcs}, NProcs;
		{finished, URL, Code, Contents} ->
			Spec = case Contents of
					   dir -> " [DIR]";
					   {redir, To} -> " -> " ++ To;
					   _ -> ""
				   end,
			io:format("~s ~s~s\n", [Code, URL, Spec]),
			case {?ENABLED(follow_dirs), ?ENABLED(follow_redirs), Contents} of
				{true, _, dir} -> Server ! {bust_dir, URL ++ "/"};
				{_, true, {redir, Target}} -> Server ! {bust_file, {URL, Target}};
				{_, _, Body} when Code =/= error, is_list(Body) ->
					spawn_link(?MODULE, found_file, [Body, URL, Server, Config]);
				_ -> ok
			end,
			NProcs - 1
	end,
	waiter(Server, Config, NewProcs).

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
