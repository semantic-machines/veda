%% @Roman Karpov <roman.karpov@gmail.com>.
%% @copyright Semantic Machines, LLC.
%% @doc veda@client basic resource code

-module(veda@client_resource).
-author('Roman Karpov <roman.karpov@gmail.com>').
-export([init/1, to_html/2, is_authorized/2, content_types_provided/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("veda@client.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
	{_, _, State2} = now(),
    {ok, Content} = hello_dtl:render([{param, integer_to_list(State2)}]),
    {Content, ReqData, State2}.
    
content_types_provided(ReqData, Context) ->
	{[{"text/html", to_html}], ReqData, Context}.

%is_authorized(ReqData, Context) ->
%    case wrq:get_req_header("authorization", ReqData) of
%        "Basic "++Base64 ->
%            Str = base64:mime_decode_to_string(Base64),
%            case string:tokens(Str, ":") of
%                ["user", "pass"] ->
%                    {true, ReqData, Context};
%                _ ->
%                    {"Basic realm=webmachine", ReqData, Context}
%            end;
%        _ ->
%            {"Basic realm=webmachine", ReqData, Context}
%    end.

is_authorized(ReqData, Context) ->
	{true, ReqData, Context}.
    
    