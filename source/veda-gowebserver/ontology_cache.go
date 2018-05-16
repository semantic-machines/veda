package main

import (
	"fmt"
	"log"
	"time"

	"strings"

	"github.com/op/go-nanomsg"
)

//ontologyRdfType is map with rdf:types included to ontology
var ontologyRdfType = map[string]bool{
	"rdfs:Class":           true,
	"rdf:Property":         true,
	"owl:Class":            true,
	"owl:ObjectProperty":   true,
	"owl:DatatypeProperty": true,
}

//tryStoreToOntologyCache checks rdf:type of individual, if its ontology class
//then it is stored to cache with individual's uri used as key
func tryStoreInOntologyCache(individual map[interface{}]interface{}) {
	uri := individual["@"].(string)
	rType := individual["rdf:type"]
	if rType != nil {
		rdfType := rType.([]interface{})
		for i := 0; i < len(rdfType); i++ {
			if ontologyRdfType[rdfType[i].(map[string]interface{})["data"].(string)] {
				ontologyCache[uri] = individual
				break
			}
		}
	} else {
		log.Println("WARN! individual not content type, uri=", uri)
	}
}

//monitorIndividualChanges is goroutine that listens to nanomsg channel
//and waits for notifications about individual changes, if ontology was changed
//then it updates cache
func monitorIndividualChanges() {
	notifyChannel, err := nanomsg.NewSubSocket()
	if err != nil {
		log.Fatal("@ERR ON CREATING UPDATE CHANNEL SOCKET: ", err)
	}

	err = notifyChannel.Subscribe("")
	if err != nil {
		log.Fatal("@ERR ON SUBSCRIBING TO UPDATES: ", err)
	}

	_, err = notifyChannel.Connect(notifyChannelURL)
	for err != nil {
		_, err = notifyChannel.Connect(notifyChannelURL)
		time.Sleep(3000 * time.Millisecond)
	}

	for {
		bytes, err := notifyChannel.Recv(0)
		if err != nil {
			log.Println("@ERR ON RECEIVING MESSAGE VIA UPDATE CHANNEL: ", err)
			continue
		}

		strdata := strings.Replace(strings.Replace(string(bytes), "#", "", -1), ";", " ", -1)
		uri := ""
		updateCounter := uint64(0)
		opID := 0
		fmt.Sscanf(strdata, "%s %d %d", &uri, &updateCounter, &opID)
		individualCache, ok := ontologyCache[uri]
		if ok {
			log.Println(individualCache)
			resourceCache := individualCache["v-s:updateCounter"].([]interface{})[0]
			updateCounterCache := resourceCache.(map[string]interface{})["data"].(uint64)
			if updateCounter > updateCounterCache {
				rr := conn.Get(false, "cfg:VedaSystem", []string{uri}, false, false)
				if rr.CommonRC != Ok {
					log.Println("@ERR ON GETTING UPDATED FROM TARANTOOL INDIVIDUAL WITH COMMON CODE: ",
						rr.CommonRC)
					continue
				} else if rr.OpRC[0] != Ok {
					log.Println("@ERR ON GETTING UPDATED FROM TARANTOOL INDIVIDUAL WITH OP CODE: ",
						rr.OpRC[0])
					continue
				}

				//individualTNT := BinobjToMap(rr.Data[0])
				individualTNT := rr.Indv[0]

				ontologyCache[uri] = individualTNT
			}
		}
	}
}
