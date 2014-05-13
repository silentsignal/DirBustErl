-module(connections_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(CT_JSON, {"application/json", to_json}).

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[?CT_JSON], ReqData, State}.

to_json(ReqData, State) ->
    {mochijson2:encode(connections()), ReqData, State}.

connections() ->
    lists:map(fun conn2json/1, ibrowse:get_metrics()).

conn2json({Host, Port, _, _, _}) ->
    [{host, list_to_binary(Host)}, {port, Port}].
