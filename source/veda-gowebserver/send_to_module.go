package main

import (
	"encoding/json"
	"log"

	"github.com/valyala/fasthttp"
)

//sendToModule handles send_to_module request
//request redirected to veda-server
//veda-server sends the given message to the given module
func sendToModule(ctx *fasthttp.RequestCtx) {
	log.Println("@SEND TO MODULE")
	log.Println("@QUERY ", string(ctx.QueryArgs().QueryString()))

	//Reading request data from context
	moduleId, _ := ctx.QueryArgs().GetUint("module_id")
	msg := string(ctx.QueryArgs().Peek("msg")[:])

	//Encoding request and send
	request := make(map[string]interface{})
	//function to execute on veda-server
	request["function"] = "send_to_module"
	//module recepient of message
	request["module_id"] = moduleId
	//message data
	request["msg"] = msg

	//Encode json request
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("@ERR SEND_TO_MODULE: ENCODE JSON REQUEST: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	log.Println(string(jsonRequest))
	socket.Send(jsonRequest, 0)
	responseBuf, _ := socket.Recv(0)

	responseJSON := make(map[string]interface{})
	err = json.Unmarshal(responseBuf, &responseJSON)
	if err != nil {
		log.Printf("@ERR SEND_TO_MODULE: DECODE JSON RESPONSE: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}
	log.Println(responseJSON)
	ctx.Response.SetStatusCode(int(responseJSON["result"].(float64)))
}
