%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc veda@core startup code

-module(veda@core).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    veda@core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the veda@core server.
start() ->
    ensure_started(crypto),
    application:start(veda@core).

%% @spec stop() -> ok
%% @doc Stop the veda@client server.
stop() ->
    Res = application:stop(veda@core),
    application:stop(crypto),
    Res.
