-module(dirbusterl_visited_urls).
-export([start_link/0, book_visit/2]).
-export([init/1, handle_call/3, terminate/2]). %% gen_server callbacks
-behavior(gen_server).


%% External API

start_link() -> gen_server:start_link(?MODULE, [], []).

book_visit(Pid, URL) -> gen_server:call(Pid, {book, URL}).


%% Callbacks for gen_server

init([]) -> {ok, {true, gb_trees:empty()}}.

handle_call({book, URL}, _From, Tree) ->
	Parts = binary:split(binary:replace(list_to_binary(URL), <<"://">>, <<"/">>), <<"/">>, [global]),
	case check_url(Parts, Tree) of
		already_visited -> {reply, false, Tree};
		{updated, NewTree} -> {reply, true, NewTree}
	end.

terminate(normal, _) -> ok.


%% Internal functions

check_url([], {true, _}) -> already_visited;
check_url([], {false, Tree}) -> {updated, {true, Tree}};
check_url([Part | Rest], {_, Parent}) ->
	case gb_trees:lookup(Part, Parent) of
		none ->
			SubTree = lists:foldr(fun (E, A) -> {false, gb_trees:insert(E, A, gb_trees:empty())} end,
				{true, gb_trees:empty()}, Rest),
			{updated, {false, gb_trees:insert(Part, SubTree, Parent)}};
		{value, {NodeStatus, _} = Child} ->
			case check_url(Rest, Child) of
				already_visited -> already_visited;
				{updated, NewTree} -> {updated, {NodeStatus, gb_trees:update(Part, NewTree, Parent)}}
			end
	end.
