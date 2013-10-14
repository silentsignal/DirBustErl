-module(url_tools).
-export([urljoin/2, subslashes/1, ensure_ends_with_slash/1]).

urljoin(Base, [$/, $/ | _] = Path) ->
	re:replace(Base, "^([^:]+:)//.*$", "\\1" ++ Path, [{return, list}]);
urljoin(Base, [$/ | _] = Path) ->
	re:replace(Base, "^([^:]+://[^/]+)/.*$", "\\1" ++ Path, [{return, list}]);
urljoin(Base, [$., $/ | Rest]) -> urljoin(Base, Rest);
urljoin(Base, [$., $., $/ | Rest]) ->
	urljoin(lists:reverse(subslashes(tl(subslashes(lists:reverse(Base))))), Rest);
urljoin(Base, Path) -> Base ++ Path.

subslashes([$/ | _] = URL) -> URL;
subslashes([_ | Rest]) -> subslashes(Rest).

ensure_ends_with_slash(Str) ->
	case lists:last(Str) of
		$/ -> Str;
		_ -> Str ++ "/"
	end.
