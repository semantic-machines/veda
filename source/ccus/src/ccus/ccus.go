package main

import (
	"github.com/gorilla/websocket"
	"log"
	"net/http"
)

const (
	ADDR string = ":8088"
)

type updateInfo struct {
	uid            string
	opid           int
	update_counter int
	cc_out chan updateInfo
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
	ws, err := websocket.Upgrade(w, r, nil, 1024, 1024)
	if _, ok := err.(websocket.HandshakeError); ok {
		http.Error(w, "Not a websocket handshake", 400)
		return
	} else if err != nil {
		return
	}
	//	log.Printf("wsHandler, err=%s", err)
	NewCcusConn(ws, ch_update_info_in)
}

var ch_ws_counter = make(chan int, 1000)

func collector_stat(ch1 chan int) {
	log.Printf("spawn stat collector")

	count_spawned := 0
	count_closed := 0

	for {
		gg := <-ch1
		log.Printf("stat collector: (%d)", gg)

		if gg > 0 {
			count_spawned = count_spawned + gg
		} else {
			count_closed = count_closed - gg
		}

		log.Printf("stat collector: total count ws connections: %d (%d)", count_spawned-count_closed, gg)
	}
}

var ch_update_info_in = make(chan updateInfo, 1000)

func collector_updateInfo(cc_in chan updateInfo) {
	log.Printf("spawn update info collector")

	_last_opid := 0
	_info_2_uid := make(map[string]updateInfo)

	for {
		arg := <-cc_in

		if arg.opid == -1 {
			// это команда на запрос last_opid
			gg1 := updateInfo{"", _last_opid, 0, nil}
			arg.cc_out <- gg1
			//log.Printf("collector:ret: last_op_id=%d", gg1.opid)
		} else if arg.update_counter == -1 {
			// это команда на запрос udate_counter по uid
			gg1 := _info_2_uid[arg.uid]

			arg.cc_out <- gg1
			//if gg1.update_counter > 0 {
			//	log.Printf("collector:ret: uid=%s opid=%d update_counter=%d", gg1.uid, gg1.opid, gg1.update_counter)
			//}
		} else {
			_info_2_uid[arg.uid] = arg
			if _last_opid < arg.opid {
				_last_opid = arg.opid
				//log.Printf("collector:set last_opid=%d", _last_opid)
			}
			//log.Printf("collector:update info: uid=%s opid=%d update_counter=%d", arg.uid, arg.opid, arg.update_counter)
		}
	}
}

func main() {

	log.SetFlags(log.LstdFlags | log.Lmicroseconds)

	go collector_updateInfo(ch_update_info_in)
	go collector_stat(ch_ws_counter)

	http.HandleFunc("/ccus", wsHandler)

	if err := http.ListenAndServe(ADDR, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}
