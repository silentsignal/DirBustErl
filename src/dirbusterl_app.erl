%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the dirbusterl application.

-module(dirbusterl_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for dirbusterl.
start(_Type, _StartArgs) ->
    dirbusterl_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for dirbusterl.
stop(_State) ->
    ok.
