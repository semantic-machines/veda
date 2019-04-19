package main

import (
	"encoding/json"
	"log"

	"github.com/itiu/fasthttp"
)

//authenticate checks validity of login and password and gives user new ticket
func authenticate(ctx *fasthttp.RequestCtx) {
	request := make(map[string]interface{})

	//fill request to veda-server
	request["function"] = "authenticate"
	login := string(ctx.QueryArgs().Peek("login")[:])
	password := string(ctx.QueryArgs().Peek("password")[:])
	secret := ctx.QueryArgs().Peek("secret")

	if len(login) < 3 {
		ctx.SetStatusCode(int(NotAuthorized))
		return
	}

	if len(secret) == 0 && len(password) < 64 {
		ctx.SetStatusCode(int(NotAuthorized))
		return
	}

	request["login"] = login
	request["password"] = password

	if len(secret) > 0 && len(secret) < 1024 {
		request["secret"] = string(secret[:])
	}

	//encode request to json
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("ERR! AUTHENTICATE: ENCODE JSON REQUEST: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//send request via nanomsg socket and reading response

	mstorage_ch_Mutex.Lock()
	NmCSend(g_mstorage_ch, jsonRequest, 0)
	responseBuf, _ := g_mstorage_ch.Recv(0)
	mstorage_ch_Mutex.Unlock()

	//decoding authenticate response json
	responseJSON := make(map[string]interface{})
	err = json.Unmarshal(responseBuf, &responseJSON)
	if err != nil {
		log.Printf("ERR! MODIFY AUTHENTICATE: DECODE JSON RESPONSE: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	//fill and encode json response	to client
	authResponse := make(map[string]interface{})
	authResponse["end_time"] = responseJSON["end_time"]
	authResponse["id"] = responseJSON["id"]
	authResponse["user_uri"] = responseJSON["user_uri"]

	jresult := responseJSON["result"]
	result := int(InternalServerError)

	switch jresult.(type) {
	case float64:
		result = int(jresult.(float64))
	case int:
		result = jresult.(int)
	}

	ctx.SetStatusCode(result)

	authResponse["result"] = result
	authResponseBuf, err := json.Marshal(authResponse)
	if err != nil {
		log.Printf("ERR! AUTHENTICATE: ENCODE JSON AUTH RESPONSE: %v\n", err)
		ctx.SetStatusCode(int(InternalServerError))
		return
	}

	//check if external users feature is enabled
	if areExternalUsers {
		//loging about external user authentication checl
		log.Printf("authenticate:check external login (%v), auth_response=%v\n", request["login"], authResponse)
		//sending get request to storage
		rr := conn.Get(false, "cfg:VedaSystem", []string{authResponse["user_uri"].(string)}, false, false)
		user := rr.GetIndv(0)
		origin, ok := getFirstString(user, "v-s:origin")

		if !ok || (ok && origin != "ExternalUser") {
			//if v-s:origin not found or value is false than return NotAuthorized
			log.Printf("ERR! login (%v) is not external, user_indv=%v\n", request["login"], user)
			authResponse["end_time"] = 0
			authResponse["id"] = ""
			authResponse["user_uri"] = ""
			authResponse["result"] = NotAuthorized
			ctx.SetStatusCode(int(NotAuthorized))
		} else if ok && origin == "ExternalUser" {
			//else set externals users ticket id to true valuse
			//			externalUsersTicketId[authResponse["user_uri"].(string)] = true
			ctx.SetStatusCode(int(Ok))
		} else {
			ctx.SetStatusCode(int(Ok))
		}
	}

	ctx.Write(authResponseBuf)
}
