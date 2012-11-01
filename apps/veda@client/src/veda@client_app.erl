%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the veda@client application.

-module(veda@client_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for veda@client.
start(_Type, _StartArgs) ->
    veda@client_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for veda@client.
stop(_State) ->
    ok.
