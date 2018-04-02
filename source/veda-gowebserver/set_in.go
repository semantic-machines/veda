package main

import (
	"encoding/json"
	"log"
	"time"

	"github.com/valyala/fasthttp"
)

//setInIndividual handles set_in_individual request
//request is redirected to veda server via socket
//veda-server set the given field to the given value
func setInIndividual(ctx *fasthttp.RequestCtx) {
	timestamp := time.Now().Unix()

	var assignedSubsystems uint64
	var ticketKey, eventID string
	// var ticket ticket

	var jsonData map[string]interface{}
	err := json.Unmarshal(ctx.Request.Body(), &jsonData)
	if err != nil {
		log.Println("@ERR PUT_INDIVIDUAL: DECODING JSON REQUEST ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	ticketKey = jsonData["ticket"].(string)
	assignedSubsystems = uint64(jsonData["assigned_subsystems"].(float64))
	eventID = jsonData["event_id"].(string)

	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		ctx.Response.SetStatusCode(int(rc))
		trail(ticket.Id, ticket.UserURI, "set_in", jsonData, "", rc, timestamp)
		return
	}

	rc = modifyIndividual("set_in", &ticket, "individuals", []map[string]interface{}{jsonData["individual"].(map[string]interface{})},
		assignedSubsystems, eventID, time.Now().Unix(), ctx)
	trail(ticket.Id, ticket.UserURI, "set_in", jsonData, "", rc, timestamp)
}
