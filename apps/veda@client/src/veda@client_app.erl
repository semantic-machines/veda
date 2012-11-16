%% @Roman Karpov <roman.karpov@gmail.com>.
%% @copyright Semantic Machines, LLC.
%% @doc veda@client app code

-module(veda@client_app).
-author('Roman Karpov <roman.karpov@gmail.com>').
-behaviour(application).
-export([start/2,stop/1]).
-include("veda@client.hrl").


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for veda@client.
start(_Type, _StartArgs) ->
    veda@client_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for veda@client.
stop(_State) ->
    ok.
