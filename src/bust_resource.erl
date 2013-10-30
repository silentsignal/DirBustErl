-module(bust_resource).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(CT_JSON, {"application/json", to_json}).


init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    {[?CT_JSON], ReqData, State}.

to_json(ReqData, State) ->
    Payload = case wrq:disp_path(ReqData) of
        "" -> dirbusterl_storage:get_busts();
        BustId -> dirbusterl_storage:get_findings(base64:decode(BustId))
    end,
    {mochijson2:encode(Payload), ReqData, State}.
