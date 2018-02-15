package main

import (
	"encoding/json"
	"log"
	"strings"
	"time"

	"github.com/valyala/fasthttp"
)

const queueStatePrefix = "srv:queue-state-"

func getIndividual(ctx *fasthttp.RequestCtx) {
	timestamp := time.Now().Unix()
	var uri string
	var ticketKey string
	var ticket ticket
	ticketKey = string(ctx.QueryArgs().Peek("ticket")[:])
	uri = string(ctx.QueryArgs().Peek("uri")[:])

	//	log.Println("\t@getIndividual: ticket=", ticketKey, ", uri=", uri)

	if len(uri) == 0 {
		log.Println("@ERR GET_INDIVIDUAL: ZERO LENGTH TICKET OR URI")
		log.Println("\t@REQUEST QUERY STRING ", string(ctx.QueryArgs().QueryString()))
		ctx.Response.SetStatusCode(int(BadRequest))
		trail("", "", "get_individual", make(map[string]interface{}), "{}", BadRequest, timestamp)
		return
	}

	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		log.Println("@ERR GET TICKET: GET_INDIVIDUAL ", rc)
		log.Println("\t@REQUEST BODY ", string(ctx.Request.Body()))
		log.Println("\t@getIndividual: ticket=", ticketKey, ", uri=", uri)
		ctx.Response.SetStatusCode(int(rc))
		trail(ticket.Id, ticket.UserURI, "get_individual", make(map[string]interface{}), "{}", rc, timestamp)
		return
	}

	if strings.Index(uri, queueStatePrefix) == 0 {
		queueName := string(uri[len(queueStatePrefix):])
		var main_queue *Queue
		var main_cs *Consumer

		main_queue_name := "individuals-flow"
		main_queue = NewQueue(main_queue_name, R)
		if !main_queue.open(CURRENT) {
			log.Println("@ERR OPENING QUEUE: ", queueName)
			ctx.Response.SetStatusCode(int(InvalidIdentifier))
			trail(ticket.Id, ticket.UserURI, "get_individual", make(map[string]interface{}), "{}", InvalidIdentifier, timestamp)
			return
		}

		// main_queue.open(CURRENT)

		main_cs = NewConsumer(main_queue, queueName)
		if !main_cs.open() {
			log.Println("@ERR OPENING CONSUMER: ", queueName)
			ctx.Response.SetStatusCode(int(InvalidIdentifier))
			trail(ticket.Id, ticket.UserURI, "get_individual", make(map[string]interface{}), "{}", InvalidIdentifier, timestamp)
			return
		}

		if !main_cs.get_info() {
			log.Println("@ERR GETTING INFO: ", queueName)
			ctx.Response.SetStatusCode(int(InternalServerError))
			trail(ticket.Id, ticket.UserURI, "get_individual", make(map[string]interface{}), "{}", InternalServerError, timestamp)
			return
		}

		individual := make(map[string]interface{})
		individual["@"] = uri
		individual["rdf:type"] = map[string]interface{}{"data": "v-s:AppInfo", "type": "Uri"}
		individual["v-s:created"] = map[string]interface{}{"data": time.Now().Format("2006-01-02T15:04:05Z"),
			"type": "Datetime"}
		individual["srv:queue"] = map[string]interface{}{"data": "srv:" + queueName, "type": "Uri"}
		individual["srv:total_count"] = map[string]interface{}{"data": main_queue.count_pushed, "type": "Integer"}
		individual["srv:current_count"] = map[string]interface{}{"data": main_cs.count_popped, "type": "Integer"}
		individualJSON, err := json.Marshal(individual)
		if err != nil {
			log.Println("@ERR GET_INDIVIDUAL: ENCODING INDIVIDUAL TO JSON ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}

		jsonArgs := map[string]interface{}{"uri": uri}
		trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, string(individualJSON), Ok, timestamp)
		ctx.Write(individualJSON)
		ctx.Response.SetStatusCode(int(Ok))
		return
	}

	individual, ok := ontologyCache[uri]
	if ok {
		individualJSON, err := json.Marshal(individual)
		if err != nil {
			log.Println("@ERR GET_INDIVIDUAL: ENCODING INDIVIDUAL TO JSON ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			return
		}

		jsonArgs := map[string]interface{}{"uri": uri}
		trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, string(individualJSON), Ok, timestamp)
		ctx.Write(individualJSON)
		ctx.Response.SetStatusCode(int(Ok))
		return
	}

	uris := make([]string, 1)
	uris[0] = uri
	jsonArgs := map[string]interface{}{"uri": uri}

	rr := conn.Get(true, ticket.UserURI, uris, false)

	if rr.CommonRC != Ok {
		log.Println("@ERR GET_INDIVIDUAL: GET INDIVIDUAL COMMON ", rr.CommonRC)
		ctx.Response.SetStatusCode(int(rr.CommonRC))
		trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, "{}", rr.CommonRC, timestamp)

		return
	} else if rr.OpRC[0] != Ok {
		ctx.Write(codeToJsonException(rr.OpRC[0]))
		ctx.Response.SetStatusCode(int(rr.OpRC[0]))
		trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, "{}", rr.OpRC[0], timestamp)
		return
	} else {
		individual = BinobjToMap(rr.Data[0])
		if individual == nil {
			log.Println("@ERR GET_INDIVIDUAL: DECODING INDIVIDUAL")
			ctx.Response.SetStatusCode(int(InternalServerError))
			trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, "{}", InternalServerError, timestamp)
			return
		}

		individualJSON, err := json.Marshal(individual)
		if err != nil {
			log.Println("@ERR GET_INDIVIDUAL: ENCODING INDIVIDUAL TO JSON ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, "{}", InternalServerError, timestamp)
			return
		}

		tryStoreInOntologyCache(individual)
		ctx.Write(individualJSON)
		trail(ticket.Id, ticket.UserURI, "get_individual", jsonArgs, string(individualJSON), Ok, timestamp)
	}

	ctx.Response.SetStatusCode(int(Ok))
	return
}

func getIndividuals(ctx *fasthttp.RequestCtx) {
	timestamp := time.Now().Unix()
	var jsonData map[string]interface{}
	var uris []string
	var ticketKey string
	var ticket ticket

	err := json.Unmarshal(ctx.Request.Body(), &jsonData)

	if err != nil {
		log.Println("@ERR GET_INDIVIDUALS: DECODING JSON REQUEST ", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		trail(ticket.Id, ticket.UserURI, "get_individuals", make(map[string]interface{}), "{}", BadRequest, timestamp)
		return
	}

	jsonArgs := map[string]interface{}{"uris": jsonData["uris"]}

	ticketKey = jsonData["ticket"].(string)

	rc, ticket := getTicket(ticketKey)
	if rc != Ok {
		ctx.Response.SetStatusCode(int(rc))
		trail(ticket.Id, ticket.UserURI, "get_individuals", jsonArgs, "{}", rc, timestamp)
		return
	}

	uris = make([]string, len(jsonData["uris"].([]interface{})))
	for i := 0; i < len(jsonData["uris"].([]interface{})); i++ {
		uris[i] = jsonData["uris"].([]interface{})[i].(string)
	}

	//if len(uris) == 0 {
	//	log.Println("@ERR GET_INDIVIDUALS: ZERO LENGTH TICKET OR URI")
	//	log.Println("\t@REQUEST BODY ", string(ctx.Request.Body()))
	//	ctx.Response.SetStatusCode(int(BadRequest))
	//	trail(ticket.Id, ticket.UserURI, "get_individuals", jsonArgs, "{}", BadRequest, timestamp)
	//	return
	//}

	individuals := make([]map[string]interface{}, 0, len(uris))
	urisToGet := make([]string, 0, len(uris))
	for i := 0; i < len(uris); i++ {
		individual, ok := ontologyCache[uris[i]]
		if ok {
			individuals = append(individuals, individual)
		} else {
			urisToGet = append(urisToGet, uris[i])
		}
	}

	/*if len(urisToGet) > 0 {
		for i := 0; i < len
		rr := conn.Get(true, ticket.UserURI, urisToGet, false)
		if rr.CommonRC != Ok {
			log.Println("@ERR GET_INDIVIDUALS: GET COMMON ", rr.CommonRC)
			ctx.Response.SetStatusCode(int(rr.CommonRC))
			return
		}

		for i := 0; i < len(rr.Data); i++ {

			// log.Println("i=", i)
			// log.Println("rr.Data[i]=", rr.Data[i])
			// log.Println("rr.OpRC[i]=", rr.OpRC[i])
			if rr.OpRC[i] == Ok {
				individual := BinobjToMap(rr.Data[i])
				if individual == nil {
					log.Println("@ERR GET_INDIVIDUALS: DECODING INDIVIDUAL")
					ctx.Response.SetStatusCode(int(InternalServerError))
					return
				}

				tryStoreInOntologyCache(individual)
				individuals = append(individuals, individual)
			}

			if err != nil {
				log.Println("@ERR ENCODING INDIVIDUAL TO JSON ", err)
				ctx.Response.SetStatusCode(int(InternalServerError))
				return
			}

		}
	}*/

	for i := 0; i < len(urisToGet); i++ {
		rr := conn.Get(true, ticket.UserURI, []string{urisToGet[i]}, false)
		if rr.CommonRC != Ok {
			log.Println("@ERR GET_INDIVIDUALS: GET COMMON ", rr.CommonRC)
			ctx.Response.SetStatusCode(int(rr.CommonRC))
			trail(ticket.Id, ticket.UserURI, "get_individuals", jsonArgs, "{}", rr.CommonRC, timestamp)
			continue
		}

		if rr.OpRC[0] == Ok {
			individual := BinobjToMap(rr.Data[0])
			if individual == nil {
				log.Println("@ERR GET_INDIVIDUALS: DECODING INDIVIDUAL")
				ctx.Response.SetStatusCode(int(InternalServerError))
				trail(ticket.Id, ticket.UserURI, "get_individuals", jsonArgs, "{}", InternalServerError, timestamp)
				return
			}

			tryStoreInOntologyCache(individual)
			individuals = append(individuals, individual)
		}

		if err != nil {
			log.Println("@ERR ENCODING INDIVIDUAL TO JSON ", err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			trail(ticket.Id, ticket.UserURI, "get_individuals", jsonArgs, "{}", InternalServerError, timestamp)
			return
		}
	}
	individualsJSON, err := json.Marshal(individuals)
	if err != nil {
		log.Println("@ERR GET_INDIVIDUALS: ENCODING INDIVIDUALS JSON ", err)
	}

	trail(ticket.Id, ticket.UserURI, "get_individuals", jsonArgs, string(individualsJSON), BadRequest, timestamp)
	ctx.Write(individualsJSON)
	ctx.Response.SetStatusCode(int(Ok))
}
