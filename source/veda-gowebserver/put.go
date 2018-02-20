package main

import (
	"encoding/json"
	"log"
	"time"
	"bytes"
	"github.com/valyala/fasthttp"
)

//putIndividual handles put_individual request
//request is redirected to veda-server via socket
//veda-server stores individual into storage
func putIndividual(ctx *fasthttp.RequestCtx) {
	timestamp := time.Now().Unix()

	var assignedSubsystems uint64
	var ticketKey, eventID string
	// var ticket ticket

	//Decoding request paramets

	var jsonData map[string]interface{}
	
	d := json.NewDecoder(bytes.NewReader(ctx.Request.Body()))
    d.UseNumber()	
	
	//err := json.Unmarshal(ctx.Request.Body(), &jsonData)
	if err := d.Decode(&jsonData); err != nil {
		log.Println("@ERR PUT_INDIVIDUAL: DECODING JSON REQUEST ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	ticketKey = jsonData["ticket"].(string)

	if jsonData["assigned_subsystems"] != nil {
		aa := jsonData["assigned_subsystems"].(json.Number)
		assignedSubsystems1, _ := aa.Int64()
		assignedSubsystems = uint64 (assignedSubsystems1)
	}
	eventID = jsonData["event_id"].(string)

	//Check if ticket is valid, if it's not valid then return fail code to client
	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		ctx.Response.SetStatusCode(int(rc))
		trail(ticket.Id, ticket.UserURI, "put", jsonData, "", rc, timestamp)
		return
	}

	//Send modify request to veda-server
	rc = modifyIndividual("put", &ticket, "individuals", []map[string]interface{}{jsonData["individual"].(map[string]interface{})},
		assignedSubsystems, eventID, time.Now().Unix(), ctx)
	trail(ticket.Id, ticket.UserURI, "put", jsonData, "", rc, timestamp)

}

//putIndividuals same to put individual but this function store array of individuals from client
func putIndividuals(ctx *fasthttp.RequestCtx) {
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
		trail(ticket.Id, ticket.UserURI, "put", jsonData, "", rc, timestamp)

		return
	}

	individualsI := jsonData["individuals"].([]interface{})
	individuals := make([]map[string]interface{}, len(individualsI))
	for i := 0; i < len(individualsI); i++ {
		individuals[i] = individualsI[i].(map[string]interface{})
	}

	rc = modifyIndividual("put", &ticket, "individuals", individuals,
		assignedSubsystems, eventID, time.Now().Unix(), ctx)
	trail(ticket.Id, ticket.UserURI, "put", jsonData, "", rc, timestamp)

}
