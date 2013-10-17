-module(url_tools).
-export([urljoin/2, subslashes/1, ensure_ends_with_slash/1]).

-define(REPLACE_LAST_PART(X, Y), lists:reverse(subslashes(lists:reverse(X)), Y)).
-define(CLEAN_LAST_PART(X), ?REPLACE_LAST_PART(X, [])).

urljoin(Base, [$/, $/ | _] = Path) ->
	re:replace(Base, "^([^:]+:)//.*$", "\\1" ++ Path, [{return, list}]);
urljoin(Base, [$/ | _] = Path) ->
	re:replace(Base, "^([^:]+://[^/]+)/.*$", "\\1" ++ Path, [{return, list}]);
urljoin(Base, [$., $/ | Rest]) ->
	urljoin(urljoin(Base, "."), Rest);
urljoin(Base, [$., $., $/ | Rest] = Path) ->
	case has_at_least_n_slashes(Base, 3) of
		true -> urljoin(urljoin(Base, ".."), Rest);
		false -> ?REPLACE_LAST_PART(Base, Path)
	end;
urljoin(Base, [Sym | Rest]) when Sym =:= $#; Sym =:= $?; Sym =:= $; ->
	case strip_symbol(lists:reverse(Base), Sym) of
		not_found -> Base ++ Rest;
		Stripped -> lists:reverse(Stripped, Rest)
	end;
urljoin(Base, "") -> Base;
urljoin(Base, ".") -> ?CLEAN_LAST_PART(Base);
urljoin(Base, "..") ->
	lists:reverse(subslashes(tl(subslashes(lists:reverse(Base)))));
urljoin(Base, Path) ->
	case lists:member($:, Path) of
		true -> Path;
		false ->
			{PathElem, Rest} = split_path(Path),
			SubSlashed = subslashes(lists:reverse(Base)),
			case is_special_path_elem(PathElem) of
				true -> lists:reverse(SubSlashed, Path);
				false -> urljoin(lists:reverse(SubSlashed, PathElem), Rest)
			end
	end.

split_path(Path) ->
	{Element, Rest} = split_path(Path, []),
	{lists:reverse(Element), Rest}.
split_path([] = L, Acc) -> {Acc, L};
split_path([$/ | Path], Acc) -> {[$/ | Acc], Path};
split_path([Char | Path], Acc) ->
	split_path(Path, [Char | Acc]).

is_special_path_elem([$? | _]) -> true;
is_special_path_elem([$# | _]) -> true;
is_special_path_elem([$; | _]) -> true;
is_special_path_elem([_ | Rest]) -> is_special_path_elem(Rest);
is_special_path_elem([]) -> false.

has_at_least_n_slashes("", _) -> false;
has_at_least_n_slashes(_, 0) -> true;
has_at_least_n_slashes([$/ | URL], N) ->
	has_at_least_n_slashes(URL, N - 1);
has_at_least_n_slashes([_ | URL], N) ->
	has_at_least_n_slashes(URL, N).

subslashes([$/ | _] = URL) -> URL;
subslashes([_ | Rest]) -> subslashes(Rest).

strip_symbol([], _) -> not_found;
strip_symbol([Symbol | _] = URL, Symbol) -> URL;
strip_symbol([_ | Rest], Symbol) -> strip_symbol(Rest, Symbol).

ensure_ends_with_slash(Str) ->
	case lists:last(Str) of
		$/ -> Str;
		_ -> Str ++ "/"
	end.
