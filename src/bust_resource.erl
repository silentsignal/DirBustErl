-module(bust_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(CT_JSON, {"application/json", to_json}).


init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[?CT_JSON], ReqData, State}.

to_json(ReqData, State) ->
    {mochijson2:encode(dirbusterl_storage:get_busts()), ReqData, State}.
