package main

import (
	"encoding/json"
	"log"
	"time"

	"github.com/op/go-nanomsg"
	"github.com/valyala/fasthttp"
)

//modifyIndividual sends request to veda server
//cmd defines function which server must execute, dataJSON is data passed to veda-server,
//assigned_subsystems are subsystem rised on this function
//event_id is current event id
func modifyIndividual(cmd string, ticket *ticket, dataKey string, dataJSON interface{}, assignedSubsystems uint64,
	eventID string, startTime int64, ctx *fasthttp.RequestCtx) ResultCode {
	timestamp := time.Now().Unix()
	request := make(map[string]interface{})

	//fills request map with parametrs
	request["function"] = cmd
	request["ticket"] = ticket.Id
	request[dataKey] = dataJSON
	request["assigned_subsystems"] = assignedSubsystems
	request["event_id"] = eventID

	//Marshal request and send to socket
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("ERR! modify individual: ENCODE JSON REQUEST, cmd=%v: request=%v, err=%v\n", cmd, request, err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return InternalServerError
	}

	responseJSON := make(map[string]interface{})

	var mstorage_ch *nanomsg.Socket
	mstorage_ch, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
	if err != nil {
		log.Println("ERR! ON CREATING SOCKET to mstorage")
		return InternalServerError
	}

	_, err = mstorage_ch.Connect(mainModuleURL)
	for err != nil {
		log.Println("ERR! ON CREATING ENDPOINT to mstorage")
		return InternalServerError
	}

	NmCSend(mstorage_ch, jsonRequest, 0)
	responseBuf, err := mstorage_ch.Recv(0)

	mstorage_ch.Close()

	if err != nil {
		log.Printf("ERR! modify individual: recieve, cmd=%v: err=%v, request=%v\n", cmd, err, request)
		ctx.Response.SetStatusCode(int(InternalServerError))
		trail(ticket.Id, ticket.UserURI, cmd, request, "{}", InternalServerError, timestamp)
		return InternalServerError
	} else {
		err = json.Unmarshal(responseBuf, &responseJSON)
		if err != nil {
			log.Printf("ERR! modify individual: DECODE JSON RESPONCE, cmd=%v: request=%v, responce=%v, err=%v\n", cmd, request, responseBuf, err)
			ctx.Response.SetStatusCode(int(InternalServerError))
			trail(ticket.Id, ticket.UserURI, cmd, request, "{}", InternalServerError, timestamp)
			return InternalServerError
		}

		//Unmarshal response data and gives response to client
		data := responseJSON["data"]

		if data != nil {
			responseData := data.([]interface{})[0].(map[string]interface{})
			ctx.Response.SetStatusCode(int(responseData["result"].(float64)))
			responseDataJSON, _ := json.Marshal(responseData)
			//log.Println("@", string(responseDataJSON))
			ctx.Write(responseDataJSON)
			trail(ticket.Id, ticket.UserURI, cmd, request, string(responseDataJSON),
				ResultCode(responseData["result"].(float64)), timestamp)
			return ResultCode(responseData["result"].(float64))
		} else {
			ctx.Response.SetStatusCode(int(responseJSON["result"].(float64)))
			responseDataJSON, _ := json.Marshal(responseJSON)
			//log.Println("@", string(responseDataJSON))
			ctx.Write(responseDataJSON)
			trail(ticket.Id, ticket.UserURI, cmd, request, string(responseDataJSON),
				ResultCode(responseJSON["result"].(float64)), timestamp)
			return ResultCode(responseJSON["result"].(float64))
		}
	}
}
