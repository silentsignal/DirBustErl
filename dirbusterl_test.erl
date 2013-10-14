-module(dirbusterl_test).
-include_lib("eunit/include/eunit.hrl").

is_dir_redir_test() ->
	?assert(dirbusterl:is_dir_redir("http://foo/bar", "http://foo/bar/")),
	?assertNot(dirbusterl:is_dir_redir("http://foo/bar", "http://qux/baz/")).

extract_paths_from_body_test() ->
	?assertEqual(
	   dirbusterl:extract_paths_from_body("<img src='foo.png'>\nDisallow: /foo/bar\n"
										  "nameg\n\n@import url('/css/styles.css');\n"),
	   ["foo.png","/foo/bar","/css/styles.css"]
	  ).
