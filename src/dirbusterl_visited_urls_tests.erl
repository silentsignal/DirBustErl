-module(dirbusterl_visited_urls_tests).
-include_lib("eunit/include/eunit.hrl").

-define(BV(U), dirbusterl_visited_urls:book_visit(P, (U))).

basic_test() ->
	{ok, P} = dirbusterl_visited_urls:start_link(),
	?assert(?BV("http://localhost/foo")),
	?assertNot(?BV("http://localhost/foo")),
	?assertNot(?BV("http://localhost/foo")),
	?assert(?BV("http://localhost/foo/bar/qux/")),
	?assertNot(?BV("http://localhost/foo")),
	?assertNot(?BV("http://localhost/foo/bar/qux/")),
	?assert(?BV("http://localhost/foo/bar/qux")),
	dirbusterl_visited_urls:stop(P).

premature_stop_test() ->
	{ok, P} = dirbusterl_visited_urls:start_link(),
	?assert(?BV("http://localhost/foo")),
	dirbusterl_visited_urls:stop(P),
	?assertExit({noproc, _}, ?BV("http://localhost/foo")).

return_test() ->
	{ok, P} = dirbusterl_visited_urls:start_link(),
	?assert(?BV("http://a/b")),
	?assert(?BV("http://a")),
	?assert(?BV("http://a/")),
	dirbusterl_visited_urls:stop(P).
