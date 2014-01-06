-module(dirbusterl_url_collector).

-behavior(gen_event).
-export([init/1, handle_event/2, terminate/2]).

init(BustId) ->
	{ok, BustId}.

handle_event({finished, URL, Code, Contents}, BustId) ->
	Metadata = case Contents of
			   dir = C -> [C];
			   {redir, Target} -> [{redir, list_to_binary(Target)}];
			   _ -> []
		   end,
	CodeFlag = if is_list(Code) -> list_to_integer(Code); true -> Code end,
	dirbusterl_storage:store_finding(BustId, list_to_binary(URL), [{code, CodeFlag} | Metadata]),
	{ok, BustId}.

terminate(_Args, _State) ->
	ok.
