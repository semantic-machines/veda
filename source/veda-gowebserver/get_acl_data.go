package main

import (
	"log"

	"github.com/valyala/fasthttp"
)

//getAclData performs request GetRightsOrigin of GetMembership, this is set by operation parametr
func getAclData(ctx *fasthttp.RequestCtx, operation uint) {
	var uri string
	var ticketKey string
	var ticket ticket

	//Readin ticket and resource uri from request context
	ticketKey = string(ctx.QueryArgs().Peek("ticket")[:])
	uri = string(ctx.QueryArgs().Peek("uri")[:])

	//If no uris passed than return BadRequest to client
	if len(uri) == 0 {
		log.Println("@ERR GET_INDIVIDUAL: ZERO LENGTH TICKET OR URI")
		ctx.Response.SetStatusCode(int(BadRequest))
		return
	}

	//Get ticket from tarantool and check its validity, if not valid then return fail code to client
	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		ctx.Response.SetStatusCode(int(rc))
		return
	}

	//Trace auth is activated only for GetRightsOrigin requset
	traceAuth := false
	if operation == GetRightsOrigin {
		traceAuth = true
	}

	//Perform authorize request to tarantool
	rr := conn.Authorize(true, ticket.UserURI, []string{uri}, operation, false, traceAuth)

	//If common response code is not Ok return fail to client
	if rr.CommonRC != Ok {
		log.Printf("@ERR GET_ACL_DATA %v: AUTH %v\n", operation, rr.CommonRC)
		ctx.Response.SetStatusCode(int(rr.CommonRC))
		return
	}

	ctx.Write([]byte(rr.Data[0]))
	ctx.Response.SetStatusCode(int(Ok))
}
