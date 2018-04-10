package main

import (
	"encoding/json"
	"log"

	"github.com/valyala/fasthttp"
)

//query function handle query request with fulltext search
//query request redirects to fr-query module via socket
func query(ctx *fasthttp.RequestCtx) {
	request := make([]interface{}, 8)
	// request := make(map[string]interface{})
	/**/
	//fills request map with parametrs
	ticketKey := string(ctx.QueryArgs().Peek("ticket")[:])
	query := string(ctx.QueryArgs().Peek("query")[:])
	sort := string(ctx.QueryArgs().Peek("sort")[:])
	databases := string(ctx.QueryArgs().Peek("databases")[:])
	reopen := ctx.QueryArgs().GetBool("reopen")
	top, _ := ctx.QueryArgs().GetUint("top")
	limit, _ := ctx.QueryArgs().GetUint("limit")
	from, _ := ctx.QueryArgs().GetUint("from")

	if top < 0 {
		top = 10000
	}

	if from < 0 {
		from = 0
	}

	if limit < 0 {
		limit = 10000
	}

	request[0] = ticketKey
	request[1] = query
	request[2] = sort
	request[3] = databases
	request[4] = reopen
	request[5] = top
	request[6] = limit
	request[7] = from

	//Marshal request and send to socket
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("@ERR QUERY INDIVIDUAL: ENCODE JSON REQUEST: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}
	querySocket.Send(jsonRequest, 0)

	responseBuf, _ := querySocket.Recv(0)
	ctx.Write(responseBuf)
	ctx.Response.SetStatusCode(int(Ok))
}
