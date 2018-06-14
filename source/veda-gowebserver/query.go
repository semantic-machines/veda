package main

import (
	"encoding/json"
	"log"

	"github.com/op/go-nanomsg"
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

	defer func() {
		if r := recover(); r != nil {
			log.Printf("ERR! fail execute(%s) request=%v\n", queryServiceURL, request)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}
	}()

	//Marshal request and send to socket
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("ERR! QUERY: ENCODE JSON REQUEST: %s %v\n", request, err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	if len(jsonRequest) == 0 {
		log.Printf("ERR! empty query: %s %v\n", request, err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	var querySocket *nanomsg.Socket
	querySocket, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
	if err != nil {
		log.Printf("ERR! ON CREATING QUERY SOCKET")
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//log.Println("use query service url: ", queryServiceURL)
	_, err = querySocket.Connect(queryServiceURL)
	if err != nil {
		log.Printf("ERR! ON CONNECT TO QUERY SOCKET, url=%s, query=%s", queryServiceURL, string(jsonRequest))
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	cb, err1 := NmCSend(querySocket, jsonRequest, 0)
	if err1 != nil || cb <= 0 {
		log.Printf("ERR! ON SEND TO FT-QUERY SOCKET, url=%s, query=%s", queryServiceURL, string(jsonRequest))
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	responseBuf, err := querySocket.Recv(0)

	querySocket.Close()

	ctx.Write(responseBuf)

	var jsonResponse map[string]interface{}
	err = json.Unmarshal(responseBuf, &jsonResponse)
	if err != nil {
		log.Printf("ERR! QUERY INDIVIDUAL: ENCODE JSON RESPONSE: %s %v\n", responseBuf, err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	rc := jsonResponse["result_code"]

	var result_code = int(InternalServerError)
	if rc != nil {

		switch rc.(type) {

		case float64:
			result_code = int(rc.(float64))
		}

		ctx.Response.SetStatusCode(result_code)
	}
}
