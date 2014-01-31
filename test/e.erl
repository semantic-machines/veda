-module(e).
-export([main/0]).

main() -> 
	{ok, Handler} = eldap:open(["172.17.1.50"],[{port, 389}]),
	ok = eldap:simple_bind(Handler, "SYK-PortalService", "123456Qw"),
	Filter = eldap:substrings("cn", [{any,"Bragina"}]),
	Result = eldap:search(Handler, [{base, "dc=neusiedler, dc=local"}, {filter, Filter}, {attributes, ["cn"]}]),
	eldap:close(Handler),
	Result.
