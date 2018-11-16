package main

import (
	"encoding/json"
	"log"
	"time"

	"github.com/itiu/fasthttp"
)

//addToIndividual handler routes request to veda-server and returns veda-server response
//veda-server adds the given value to ght given field
func addToIndividual(ctx *fasthttp.RequestCtx) {
	timestamp := time.Now()

	var assignedSubsystems uint64
	var ticketKey, eventID string
	// var ticket ticket

	//jsonData is decoded from http request from client
	var jsonData map[string]interface{}
	err := json.Unmarshal(ctx.Request.Body(), &jsonData)
	if err != nil {
		log.Println("ERR! PUT_INDIVIDUAL: DECODING JSON REQUEST ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	if jsonData["ticket"] != nil {
	    ticketKey = jsonData["ticket"].(string)
	}

	if jsonData["assigned_subsystems"] != nil {
	    assignedSubsystems = uint64(jsonData["assigned_subsystems"].(float64))
	}

	eventID = jsonData["event_id"].(string)

	//request ticket with the given key and check its validity
	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		//if ticket is invalid return fail code
		ctx.Response.SetStatusCode(int(rc))
		trail(ticket.Id, ticket.UserURI, "add_to", jsonData, "", rc, timestamp)
		return
	}

	//send modify request to veda-server
	rc = modifyIndividual("add_to", &ticket, "individuals", []map[string]interface{}{jsonData["individual"].(map[string]interface{})},
		assignedSubsystems, eventID, time.Now().Unix(), ctx)
	trail(ticket.Id, ticket.UserURI, "add_to", jsonData, "", rc, timestamp)
}
