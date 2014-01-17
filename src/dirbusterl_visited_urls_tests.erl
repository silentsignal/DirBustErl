-module(dirbusterl_visited_urls_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
	{ok, P} = dirbusterl_visited_urls:start_link(),
	?assert(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo")),
	?assertNot(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo")),
	?assertNot(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo")),
	?assert(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo/bar/qux/")),
	?assertNot(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo")),
	?assertNot(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo/bar/qux/")),
	?assert(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo/bar/qux")),
	dirbusterl_visited_urls:stop(P).

premature_stop_test() ->
	{ok, P} = dirbusterl_visited_urls:start_link(),
	?assert(dirbusterl_visited_urls:book_visit(P, "http://localhost/foo")),
	dirbusterl_visited_urls:stop(P),
	?assertExit({noproc, _}, dirbusterl_visited_urls:book_visit(P, "http://localhost/foo")).
