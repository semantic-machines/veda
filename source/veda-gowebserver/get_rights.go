package main

import (
	"encoding/json"
	"log"

	"github.com/itiu/fasthttp"
)

const (
	//AccessCanCreate is create permission
	AccessCanCreate uint8 = 1 << 0
	//AccessCanRead is read permission
	AccessCanRead uint8 = 1 << 1
	//AccessCanUpdate is update permission
	AccessCanUpdate uint8 = 1 << 2
	//AccessCanDelete is delete permission
	AccessCanDelete uint8 = 1 << 3
	//AccessCanNotCreate is create restriction
	AccessCanNotCreate uint8 = 1 << 4
	//AccessCanNotRead is read restriction
	AccessCanNotRead uint8 = 1 << 5
	//AccessCanNotUpdate is update restriction
	AccessCanNotUpdate uint8 = 1 << 6
	//AccessCanNotDelete is delete restriction
	AccessCanNotDelete uint8 = 1 << 7
	//DefaultAccess is full CRUD access
	DefaultAccess uint8 = 15
)

//getRights is handler for get_rigths request
func getRights(ctx *fasthttp.RequestCtx) {
	var uri string
	var ticketKey string
	var ticket ticket

	//Get parametrs from request context
	ticketKey = string(ctx.QueryArgs().Peek("ticket")[:])
	uri = string(ctx.QueryArgs().Peek("uri")[:])

	//If no uri passed then return BadRequest
	if len(uri) == 0 {
		log.Println("ERR! GET_INDIVIDUAL: ZERO LENGTH TICKET OR URI")
		ctx.Response.SetStatusCode(int(BadRequest))
		return
	}

	//Check if ticket is valid and return fail if not
	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		ctx.Response.SetStatusCode(int(rc))
		return
	}

	//Send authorize request to tarantool
	rr := conn.Authorize(true, ticket.UserURI, uri, Authorize, false, false)

	//If common response code is not Ok return fail to client
	if rr.CommonRC != Ok {
		log.Println("ERR! GET RIGHS: AUTH ", rr.CommonRC)
		ctx.Response.SetStatusCode(int(rr.CommonRC))
		return
	}

	//Read access and get bytes of CRUD
	access := rr.Rights[0]
	canCreate := (access & AccessCanCreate) > 0
	canRead := (access & AccessCanRead) > 0
	canUpdate := (access & AccessCanUpdate) > 0
	canDelete := (access & AccessCanDelete) > 0

	//Create individual with boolean resource for each byte of CRUD
	individual := make(map[string]interface{})
	individual["@"] = "_"
	individual["rdf:type"] = []interface{}{map[string]interface{}{"type": "Uri", "data": "v-s:PermissionStatement"}}
	if canCreate {
		individual["v-s:canCreate"] = []interface{}{map[string]interface{}{"type": "Boolean", "data": canCreate}}
	}

	if canRead {
		individual["v-s:canRead"] = []interface{}{map[string]interface{}{"type": "Boolean", "data": canRead}}
	}

	if canUpdate {
		individual["v-s:canUpdate"] = []interface{}{map[string]interface{}{"type": "Boolean", "data": canUpdate}}
	}

	if canDelete {
		individual["v-s:canDelete"] = []interface{}{map[string]interface{}{"type": "Boolean", "data": canDelete}}
	}

	//Marshal individual to hson and give response to client
	individualJSON, err := json.Marshal(individual)
	if err != nil {
		log.Println("ERR! GET_INDIVIDUAL: ENCODING INDIVIDUAL TO JSON ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	ctx.Response.SetStatusCode(int(Ok))
	ctx.Write(individualJSON)
}
