package main

import (
	"encoding/json"
	"log"
	"os"
	"runtime"
	"runtime/pprof"

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

	if moduleId == 0 {
		if msg == "start_cpuprofile" {
			cpuprofile := "gowebserver-cpu.prof"
			f, err := os.Create(cpuprofile)
			if err != nil {
				log.Fatal("could not create CPU profile: ", err)
			}

			if err := pprof.StartCPUProfile(f); err != nil {
				log.Fatal("could not start CPU profile: ", err)
			}
		} else if msg == "stop_cpuprofile" {
			pprof.StopCPUProfile()
		} else if msg == "snap_memprofile" {
			memprofile := "gowebserver-mem.prof"
			f, err := os.Create(memprofile)
			if err != nil {
				log.Fatal("could not create memory profile: ", err)
			}
			runtime.GC() // get up-to-date statistics
			if err := pprof.WriteHeapProfile(f); err != nil {
				log.Fatal("could not write memory profile: ", err)
			}
			f.Close()
		}

		return
	}

	//Encode json request
	jsonRequest, err := json.Marshal(request)
	if err != nil {
		log.Printf("@ERR SEND_TO_MODULE: ENCODE JSON REQUEST: %v\n", err)
		ctx.Response.SetStatusCode(int(InternalServerError))
		return
	}

	log.Println(string(jsonRequest))
	g_mstorage_ch.Send(jsonRequest, 0)
	responseBuf, _ := g_mstorage_ch.Recv(0)

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
