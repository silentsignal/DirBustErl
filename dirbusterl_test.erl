-module(dirbusterl_test).
-include_lib("eunit/include/eunit.hrl").

urljoin_test() ->
	?assertEqual(
	   dirbusterl:urljoin("http://foo/bar", "http://another/baz"),
	   "http://another/baz"),
	?assertEqual(
	   dirbusterl:urljoin("https://foo/bar", "https://another/baz"),
	   "https://another/baz"),
	?assertEqual(
	   dirbusterl:urljoin("http://foo/bar/qux/", "../baz"),
	   "http://foo/bar/baz"),
	?assertEqual(
	   dirbusterl:urljoin("http://foo/bar/qux/", "./baz"),
	   "http://foo/bar/qux/baz").

ensure_ends_with_slash_test() ->
	?assertEqual(
	   dirbusterl:ensure_ends_with_slash("http://foo/bar"),
	   "http://foo/bar/"),
	?assertEqual(
	   dirbusterl:ensure_ends_with_slash("http://foo/bar/"),
	   "http://foo/bar/").

is_dir_redir_test() ->
	?assert(dirbusterl:is_dir_redir("http://foo/bar", "http://foo/bar/")),
	?assertNot(dirbusterl:is_dir_redir("http://foo/bar", "http://qux/baz/")).

extract_paths_from_body_test() ->
	?assertEqual(
	   dirbusterl:extract_paths_from_body("<img src='foo.png'>\nDisallow: /foo/bar\n"
										  "nameg\n\n@import url('/css/styles.css');\n"),
	   ["foo.png","/foo/bar","/css/styles.css"]
	  ).
