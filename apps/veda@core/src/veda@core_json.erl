-module(veda@core_json).
-include("veda@core.hrl").
-export([from_json/1, from_json/2, to_json/1, to_json/2]).

to_json(PropList) ->
  to_json(PropList, fun(_) -> true end).

to_json(PropList, IsStrFun) ->
  list_to_binary(mochijson2:encode(from_proplist(PropList, IsStrFun))).

from_json(Json) ->
  from_json(Json, fun(_) -> true end).

from_json(Json, IsStrFun) ->
  to_proplist(mochijson2:decode(Json), IsStrFun).
  
from_proplist(List=[H|_], IsStrFun) when is_tuple(H) ->
  { struct, lists:map(fun(P) -> from_proplist(P, IsStrFun) end, List) };
from_proplist({PropName, ComplexProp=[H|_]}, IsStrFun) when is_tuple(H) ->
  { list_to_binary(atom_to_list(PropName)), from_proplist(ComplexProp, IsStrFun) };
from_proplist({PropName, PropVal}, IsStrFun) ->
  { list_to_binary(atom_to_list(PropName)), to_value(PropName, PropVal, IsStrFun) }.

to_proplist({struct, PropList}, IsStrFun) when is_list(PropList) ->
  lists:map(fun(P) -> to_proplist(P, IsStrFun) end, PropList);
to_proplist({PropName, ComplexProp={struct, _}}, IsStrFun) ->
  { list_to_atom(binary_to_list(PropName)), to_proplist(ComplexProp, IsStrFun) };
to_proplist({PropName, PropVal}, IsStrFun) ->
  PropAtom = list_to_atom(binary_to_list(PropName)),
  { PropAtom, from_value(PropAtom, PropVal, IsStrFun) }.

to_value(PropName, L=[H|_], IsStrFun) when is_list(L) and is_list(H) ->
  lists:map(fun(P) -> to_value(PropName, P, IsStrFun) end, L);
to_value(PropName, L, IsStrFun) when is_list(L) ->
  case IsStrFun(PropName) of
    true -> list_to_binary(L);
    _ -> lists:map(fun(V) -> to_value(PropName, V, IsStrFun) end, L)
  end;
to_value(_, V, _) ->
  V.

from_value(PropName, L, IsStrFun) when is_list(L) ->
  lists:map(fun(P) -> from_value(PropName, P, IsStrFun) end, L);
from_value(PropName, B, IsStrFun) when is_binary(B) ->
  case IsStrFun(PropName) of
    true -> binary_to_list(B);
    _ -> B
  end;
from_value(_, V, _) ->
  V.