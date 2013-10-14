-module(url_tools).
-export([urljoin/2, subslashes/1, ensure_ends_with_slash/1]).

urljoin(Base, [$/, $/ | _] = Path) ->
	re:replace(Base, "^([^:]+:)//.*$", "\\1" ++ Path, [{return, list}]);
urljoin(Base, [$/ | _] = Path) ->
	re:replace(Base, "^([^:]+://[^/]+)/.*$", "\\1" ++ Path, [{return, list}]);
urljoin(Base, [$., $/ | Rest]) ->
	urljoin(lists:reverse(subslashes(lists:reverse(Base))), Rest);
urljoin(Base, [$., $., $/ | Rest]) ->
	urljoin(urljoin(Base, ".."), Rest);
urljoin(Base, [Sym | Rest]) when Sym =:= $#; Sym =:= $?; Sym =:= $; ->
	case strip_symbol(lists:reverse(Base), Sym) of
		not_found -> Base ++ Rest;
		Stripped -> lists:reverse(Stripped, Rest)
	end;
urljoin(Base, "") -> Base;
urljoin(Base, ".") -> lists:reverse(subslashes(lists:reverse(Base)));
urljoin(Base, "..") ->
	lists:reverse(subslashes(tl(subslashes(lists:reverse(Base)))));
urljoin(Base, Path) ->
	case lists:member($:, Path) of
		true -> Path;
		false ->
			Parent = subslashes(lists:reverse(Base)),
			lists:reverse(Parent, if Path =:= "." -> ""; true -> Path end)
	end.

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
