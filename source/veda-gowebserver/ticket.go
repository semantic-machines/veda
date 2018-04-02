package main

import (
	"encoding/json"
	"log"
	"strconv"
	//"strings"
	"time"

	"github.com/valyala/fasthttp"
)

//getTicket handles get_ticket request
//it requests ticket from tarantool by the given id
//and checks its validity
func getTicket(ticketKey string) (ResultCode, ticket) {
	var ticket ticket

	//if no ticket id or systicket passed then change it to guest
	if ticketKey == "" || ticketKey == "systicket" {
		ticketKey = "guest"
		ticket.Id = "guest"
		ticket.UserURI = "cfg:Guest"
		ticket.result = Ok
		return Ok, ticket
	}

	//Look for ticket in cache
	if ticketCache[ticketKey].Id != "" {
		//If found check expiration time
		ticket = ticketCache[ticketKey]
		if time.Now().Unix() > ticket.EndTime {
			//If expired, delete and return
			delete(ticketCache, ticketKey)
			log.Printf("@TICKET %v FROM USER %v EXPIRED: START %v END %v NOW %v\n", ticket.Id, ticket.UserURI,
				ticket.StartTime, ticket.EndTime, time.Now().Unix())
			return TicketExpired, ticket
		}
	} else {
		//If not found, request it from tarantool
		rr := conn.GetTicket([]string{ticketKey}, false)
		//If common response code is not Ok return fail code
		if rr.CommonRC != Ok {
			log.Println("@ERR ON GET TICKET FROM TARANTOOL")
			return InternalServerError, ticket
		}

		//If operation code is not Ok return fail code
		if rr.OpRC[0] != Ok {
			return rr.OpRC[0], ticket
		}

		individual := BinobjToMap(rr.Data[0])
		if individual == nil {
			log.Println("@ERR GET_TICKET0: DECODING TICKET")
			return InternalServerError, ticket
		}

		var duration int64

		ticket.UserURI, _ = getFirstString(individual, "ticket:accessor")
		tt, _ := getFirstString(individual, "ticket:when")
		mask := "2006-01-02T15:04:05.00000000"
		startTime, _ := time.Parse(mask[0:len(tt)], tt)
		ticket.StartTime = startTime.Unix()
		dd, _ := getFirstString(individual, "ticket:duration")
		duration, _ = strconv.ParseInt(dd, 10, 64)

		ticket.Id = ticketKey
		ticket.EndTime = ticket.StartTime + duration

		//Save ticket in the cache
		ticketCache[ticketKey] = ticket
	}

	if areExternalUsers {
		//If external users feature is enabled
		log.Printf("check external user (%s)\n", ticket.UserURI)
		//Check if ticket exists
		_, ok := externalUsersTicketId[ticket.Id]
		if !ok {
			//If ticket not found then get user from tarantool and decode it
			rr := conn.Get(false, "cfg:VedaSystem", []string{ticket.UserURI}, false)
			user := BinobjToMap(rr.Data[0])
			//Check its field v-s:origin
			data, ok := user["v-s:origin"]
			if !ok || (ok && !data.(map[string]interface{})["data"].(bool)) {
				//If this field not found or it contains false then return error code
				log.Printf("ERR! user (%s) is not external\n", ticket.UserURI)
				ticket.Id = "?"
				ticket.result = NotAuthorized
			} else if ok && data.(map[string]interface{})["data"].(bool) {
				//Else store ticket to cache
				log.Printf("user is external (%s)\n", ticket.UserURI)
				externalUsersTicketId[ticket.UserURI] = true
			}
		}
	}

	return Ok, ticket
}

//isTicketValid handles is_ticket_valid request
func isTicketValid(ctx *fasthttp.RequestCtx) {
	var ticketKey string
	//Decode parametrs from requestn context
	ticketKey = string(ctx.QueryArgs().Peek("ticket")[:])
	//Get ticket with the given id from tarantool
	rc, _ := getTicket(ticketKey)
	//If errored but NotTicketExpired return error to client
	if rc != Ok && rc != TicketExpired {
		ctx.Write(codeToJsonException(rc))
		ctx.Response.SetStatusCode(int(rc))
		return
	} else if rc == TicketExpired {
		//If TicketExpired then return false
		ctx.Write([]byte("false"))
		ctx.Response.SetStatusCode(int(rc))
		return
	}

	//If ticket with Ok rc, return true
	ctx.Write([]byte("true"))
	ctx.Response.SetStatusCode(int(Ok))
}

//getTicketTrusted handles get_ticket_trusted request
func getTicketTrusted(ctx *fasthttp.RequestCtx) {
	log.Println("@GET TICKET TRUSTED")
	var ticketKey, login string

	//Read params from request context
	ticketKey = string(ctx.QueryArgs().Peek("ticket")[:])
	login = string(ctx.QueryArgs().Peek("login")[:])

	//Prepare request to veda server with function get_ticket_trusted
	request := make(map[string]interface{})
	request["function"] = "get_ticket_trusted"
	request["ticket"] = ticketKey
	request["login"] = login

	//Marshal json request
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("@ERR GET_TICKET_TRUSTED: ENCODE JSON REQUEST: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//Send request to veda server and read response
	socket.Send(jsonRequest, 0)
	responseBuf, _ := socket.Recv(0)
	responseJSON := make(map[string]interface{})
	err = json.Unmarshal(responseBuf, &responseJSON)
	if err != nil {
		log.Printf("@ERR GET_TICKET_TRUSTED: DECODE JSON RESPONSE: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//Decoding resoinse
	getTicketResponse := make(map[string]interface{})
	getTicketResponse["end_time"] = responseJSON["end_time"]
	getTicketResponse["id"] = responseJSON["id"]
	getTicketResponse["user_uri"] = responseJSON["user_uri"]
	getTicketResponse["result"] = responseJSON["result"]

	//Encoding json response and retiurn to client
	getTicketResponseBuf, err := json.Marshal(getTicketResponse)
	if err != nil {
		log.Printf("@ERR GET_TICKET_TRUSTED: ENCODE JSON RESPONSE: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	ctx.SetStatusCode(int(responseJSON["result"].(float64)))
	ctx.Write(getTicketResponseBuf)
}
