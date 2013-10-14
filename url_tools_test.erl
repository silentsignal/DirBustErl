-module(url_tools_test).
-include_lib("eunit/include/eunit.hrl").

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

ensure_ends_with_slash_test() ->
	?assertEqual(
	   url_tools:ensure_ends_with_slash("http://foo/bar"),
	   "http://foo/bar/"),
	?assertEqual(
	   url_tools:ensure_ends_with_slash("http://foo/bar/"),
	   "http://foo/bar/").
