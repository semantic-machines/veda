%% @Roman Karpov <roman.karpov@gmail.com>.
%% @copyright Semantic Machines, LLC.


-module(veda@core_app).

-include("veda@core.hrl").
-author('Roman Karpov <roman.karpov@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    veda@core_sup:start_link().

stop(_State) ->
    ok.
