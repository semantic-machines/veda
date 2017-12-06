package main

import (
	"log"
	"net"
	"time"

	"fmt"

	"github.com/valyala/fasthttp"
)

//query function handle query request with fulltext search
//query request redirects to fr-query module via socket
func query(ctx *fasthttp.RequestCtx) {
	timestamp := time.Now().Unix()

	//Creates tcp socket connection to ft-query module
	socket, err := net.Dial("tcp", ftQueryURL)
	if err != nil {
		log.Println("@ERR QUERY: ERR ON DIAL ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//Get request parametrs from context
	ticketKey := string(ctx.QueryArgs().Peek("ticket")[:])
	query := string(ctx.QueryArgs().Peek("query")[:])
	sort := string(ctx.QueryArgs().Peek("sort")[:])
	databases := string(ctx.QueryArgs().Peek("databases")[:])
	reopen := ctx.QueryArgs().GetBool("reopen")
	top, _ := ctx.QueryArgs().GetUint("top")
	limit, _ := ctx.QueryArgs().GetUint("limit")
	from, _ := ctx.QueryArgs().GetUint("from")

	jsonArgs := map[string]interface{}{
		"ticket":    ticketKey,
		"query":     query,
		"sort":      sort,
		"databases": databases,
		"reopen":    reopen,
		"top":       top,
		"limit":     limit,
		"from":      from,
	}

	//Check if ticket is valid, if not then return fail code to client
	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		ctx.Response.SetStatusCode(int(rc))
		trail(ticket.Id, ticket.UserURI, "query", jsonArgs, "", rc, timestamp)
		return
	}

	//Create request string to send it to ft-query
	request := fmt.Sprintf("%v�%v�%v�%v�%v�%v�%v�%v", ticketKey, query, sort, databases, reopen,
		top, limit, from)

	//Encoding request size and request to buffer
	requestSize := uint32(len(request))
	buf := make([]byte, 4, 4+requestSize)
	buf[0] = byte((requestSize >> 24) & 0xFF)
	buf[1] = byte((requestSize >> 16) & 0xFF)
	buf[2] = byte((requestSize >> 8) & 0xFF)
	buf[3] = byte(requestSize & 0xFF)
	buf = append(buf, []byte(request)...)

	n := 0
	//Sending buffer while whole buffer is not sent
	for n < len(buf) {
		var sent int
		sent, err = socket.Write(buf[n:])
		if err != nil {
			log.Println("@ERR QUERY: ERR SENDING REQUEST ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}
		n += sent
	}

	//Reading response size
	buf = make([]byte, 4)
	n, err = 0, nil
	for n < 4 {
		var read int
		read, err = socket.Read(buf[n:])
		if err != nil {
			log.Println("@ERR QUERY: ERR READING RESPONSE SIZE ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}
		n += read
	}

	//Decoding response
	responseSize := uint32(0)
	for i := 0; i < 4; i++ {
		responseSize = (responseSize << 8) + uint32(buf[i])
	}

	//Allocating response buffer
	response := make([]byte, responseSize)

	//Reading response to buffer until whole response is read
	response = make([]byte, responseSize)
	n, err = 0, nil
	for n < int(responseSize) {
		var read int
		read, err = socket.Read(response[n:])
		if err != nil {
			log.Println("@ERR QUERY: ERR READING RESPONSE ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}
		n += read
	}

	socket.Close()

	trail(ticket.Id, ticket.UserURI, "query", jsonArgs, string(response), Ok, timestamp)
	//Return result to client
	ctx.Response.SetStatusCode(int(Ok))
	ctx.Write(response)
}
