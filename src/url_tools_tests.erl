-module(url_tools_tests).
-include_lib("eunit/include/eunit.hrl").

-define(RFC_1808_BASE, "http://a/b/c/d;p?q#f").

urljoin_test() ->
	?assertEqual(
	   url_tools:urljoin("http://foo/bar", "http://another/baz"),
	   "http://another/baz"),
	?assertEqual(
	   url_tools:urljoin("https://foo/bar", "https://another/baz"),
	   "https://another/baz"),
	?assertEqual(
	   url_tools:urljoin("http://foo/bar/qux/", "../baz"),
	   "http://foo/bar/baz"),
	?assertEqual(
	   url_tools:urljoin("http://foo/bar/qux/", "./baz"),
	   "http://foo/bar/qux/baz").

urljoin_rfc1808_test() ->
	{ok, FP} = file:open("rfc1808.txt", [read, raw, read_ahead, binary]),
	read_rfc1808_lines_until_51(FP),
	test_rfc1808_lines(FP),
	file:close(FP).

read_rfc1808_lines_until_51(FP) ->
	{ok, Line} = file:read_line(FP),
	case Line of
		<<"5.1.", _/binary>> -> file:read_line(FP);
		_ -> read_rfc1808_lines_until_51(FP)
	end.

test_rfc1808_lines(FP) ->
	case file:read_line(FP) of
		{ok, Line} ->
			case re:run(Line, "^\\ +([^ ]+)\\ +=\\ <URL:([^>]+)>$",
						[{capture, all_but_first, list}]) of
				{match, [Input, Expected]} ->
					Path = if Input =:= "<>" -> ""; true -> Input end,
					Output = url_tools:urljoin(?RFC_1808_BASE, Path),
					?assertEqual(Expected, Output);
				_ -> not_a_test
			end,
			test_rfc1808_lines(FP);
		eof -> ok
	end.

ensure_ends_with_slash_test() ->
	?assertEqual(
	   url_tools:ensure_ends_with_slash("http://foo/bar"),
	   "http://foo/bar/"),
	?assertEqual(
	   url_tools:ensure_ends_with_slash("http://foo/bar/"),
	   "http://foo/bar/").
