-module(dirbusterl).
-compile([export_all]).

-define(WORDLIST, "/home/dnet/_app/DirBuster-0.12/directory-list-2.3-small.txt").
-define(URL_PACKETS, 16).
-define(WORKERS, 16).
-define(TRIES, 4).

bust(URL) ->
	{ok, WordList} = file:open(?WORDLIST, [read, raw, read_ahead, binary]),
	BaseURL = ensure_ends_with_slash(URL),
	Workers = [spawn_link(dirbusterl, worker, [self(), BaseURL, get_packet(WordList)])
			   || _ <- lists:seq(1, ?WORKERS)],
	burst_wordlist(WordList),
	[Pid ! quit || Pid <- Workers],
	file:close(WordList).

ensure_ends_with_slash(Str) ->
	case lists:reverse(Str) of
		 [$/ | _] -> Str;
		 WithoutSlash -> lists:reverse([$/ | WithoutSlash])
	end.

burst_wordlist(WordList) ->
	receive
		{ready, Pid} ->
			case get_packet(WordList) of
				<<>> -> ok;
				Packet ->
					Pid ! {packet, Packet},
					burst_wordlist(WordList)
			end;
		Msg -> io:format("Unknown message in burst: ~p~n", [Msg])
	end.

get_packet(WordList) -> get_packet(WordList, ?URL_PACKETS, <<>>).
get_packet(_, 0, Acc) -> Acc;
get_packet(WordList, Count, Acc) ->
	case file:read_line(WordList) of
		{ok, Line} ->
			NextAcc = case Line of
						  <<$#, _/binary>> -> Acc; %% ignore comments
						  _ -> <<Acc/binary, Line/binary>>
					  end,
			get_packet(WordList, Count - 1, NextAcc);
		eof -> Acc
	end.

worker(Server, BaseURL, Packet) ->
	try_urls(BaseURL, binary:split(Packet, <<$\n>>, [global, trim])),
	Server ! {ready, self()},
	receive
		{packet, NewPacket} -> worker(Server, BaseURL, NewPacket);
		quit -> ok;
		Msg -> io:format("Unknown message in worker: ~p~n", [Msg])
	end.

try_urls(_, []) -> ok;
try_urls(BaseURL, [Path | Rest]) ->
	ReqURL = BaseURL ++ binary_to_list(Path),
	try_url(ReqURL),
	try_urls(BaseURL, Rest).

try_url(URL) -> try_url(URL, ?TRIES).
try_url(URL, N) ->
	case ibrowse:send_req(URL, [], get, [], [], infinity) of
		{ok, "404", _, _} -> ok;
		{ok, Code = [$3 | _], Headers, _} -> io:format("~s ~s -> ~s\n", [Code, URL, get_location(Headers)]);
		{ok, Code, _, _} -> io:format("~s ~s\n", [Code, URL]);
		{error, _} when N > 0 -> try_url(URL, N - 1)
	end.

get_location([]) -> "N/A";
get_location([{Header, Value} | Rest]) ->
	case string:to_lower(Header) of
		"location" -> Value;
		_ -> get_location(Rest)
	end.
