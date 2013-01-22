%% @Roman Karpov <roman.karpov@gmail.com>.
%% @copyright Semantic Machines, LLC.

-module(veda@core_server).
-include("veda@core.hrl").
-author('Roman Karpov <roman.karpov@gmail.com>').
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(struct, {lst=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).%, get_auth_ticket/1]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	{ok, App} = application:get_application(?MODULE),
    {ok, [{pacahon_endpoint, PacahonAddress}]} = file:consult(filename:join([priv_dir(App), "veda@core.conf"])),
    {ok, Context} = erlzmq:context(),
    {ok, Pacahon} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Pacahon, PacahonAddress),
    {ok, AuthRequest} = file:read_file(filename:join([priv_dir(App), "_auth_request.json"])), % Считать из файла и отправить запрос аутентификации
    ok = erlzmq:send(Pacahon, AuthRequest, []),
    {ok, AuthResponse} = erlzmq:recv(Pacahon), % Получить и записать в файл ответ на запрос аутентификации
    file:write_file(filename:join([priv_dir(App), "_auth_response.json"]), AuthResponse),
  
	%{match, [AuthTicket]} = re:run(AuthResponse, <<"auth:.{8}-.{4}-.{4}-.{4}-.{12}">>, [{capture,first,binary}]), % Извлечь подстроку вида "auth:619eeb00-e79f-49d1-91dc-4028b309069b"
    DecodedResponse = mochijson2:decode(AuthResponse),
    MsgResult = proplists:get_value(<<"msg:result">>, DecodedResponse#struct.lst),
    AuthTicket = proplists:get_value(<<"auth:ticket">>, MsgResult#struct.lst),
  
    io:format("Auth ticket: ~p~n", [AuthTicket]),
    ok = erlzmq:close(Pacahon),
    ok = erlzmq:term(Context),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PacahonAddress], []).
    


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([PacahonAddress]) ->
	%io:format("~p~p~n", [?MODULE, PacahonAddress]),
	{ok, PacahonAddress}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------




priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.
