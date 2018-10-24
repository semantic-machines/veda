package main

import (
	"encoding/json"
	"log"
	//"strconv"
	"time"
)

//trail stores data about request in traildb
func trail1(ticketId, userId, action string, args string, result string, resultCode ResultCode, startTime time.Time) {
	if !isTrail {
		return
	}

	timestamp := time.Now()
	delta := timestamp.Sub(startTime)
	ns_delta := delta.Nanoseconds()/1000000
	
	log.Printf("TRAIL(%d): action=%s; time=%s; code=%s; ticket=%s; user=%s;\nargs=%s\n", to_delta_level (ns_delta), action, delta.String (), resultCodeToStr(resultCode), ticketId, userId, args)
}

func trail(ticketId, userId, action string, args map[string]interface{}, result string, resultCode ResultCode, startTime time.Time) {
	if !isTrail {
		return
	}

	timestamp := time.Now()
	jsonArgs, _ := json.Marshal(args)
//	log.Printf("TRAIL:time=%s;ticket=%s;user=%s;action=%s;\nargs=%s;\nres=%s;\ncode=%s\n\n", strconv.FormatInt(timestamp-startTime, 10), ticketId, userId, action, string(jsonArgs), result,
//		string(codeToJsonException(resultCode)))
	delta := timestamp.Sub(startTime)
	ns_delta := delta.Nanoseconds()/1000000
		
	log.Printf("TRAIL(%d): action=%s; time=%s; code=%s; ticket=%s; user=%s;\nargs=%s\n", to_delta_level (ns_delta), action, delta.String (), resultCodeToStr(resultCode), ticketId, userId, string(jsonArgs))
}

func to_delta_level (ns_delta int64) int {
	var delta_level int
	
	if ns_delta >= 0 && ns_delta <= 1 {
		delta_level = 0 
	} else if ns_delta > 1 && ns_delta <= 10 {
		delta_level = 1 		
	} else if ns_delta > 10 && ns_delta <= 100 {
		delta_level = 2 		
	} else if ns_delta > 100 && ns_delta <= 1000 {
		delta_level = 3 		
	} else if ns_delta > 1000 && ns_delta <= 10000 {
		delta_level = 4 		
	} else if ns_delta > 10000 && ns_delta <= 100000 {
		delta_level = 5 		
	} else if ns_delta > 100000 {
		delta_level = 6 		
	}
	return 	delta_level
}

func resultCodeToStr(code ResultCode) string {
        
        switch code {
        case Ok:
                return  "Ok"
        case BadRequest:
                return "BadRequest"
        case NotAuthorized:
                return "NotAuthorized"
        case NotFound:
                return "NotFound"
        case InternalServerError:
                return "InternalServerError"
        case TicketExpired:
                return "TicketExpired"
        case NoContent:
                return "NoContent"
        case SizeTooLarge:
                return "SizeToLarge"
        case UnprocessableEntity:
                return "UnprocessableEntity"
        default:
                return "UnknownError"
        }
}
