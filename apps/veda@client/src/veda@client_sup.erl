%% @Roman Karpov <roman.karpov@gmail.com>.
%% @copyright Semantic Machines, LLC.
%% @doc Supervisor for the veda@client application.


-module(veda@client_sup).
-author('Roman Karpov <roman.karpov@gmail.com>').
-behaviour(supervisor).
-include("veda@client.hrl").

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, [{ip_address, Ip},{ip_port, Port}]} = file:consult(filename:join([priv_dir(App), "veda@client.conf"])),
    {ok, Dispatch} = file:consult(filename:join([priv_dir(App), "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, filename:join([priv_dir(App), "log"])},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.

%%
%% @doc return the priv dir
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.
