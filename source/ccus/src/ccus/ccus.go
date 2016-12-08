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
	NewCcusConn(ws, chan_update_info)
}

var ch1 = make(chan int, 1000)

func collector_stat(ch1 chan int) {
	log.Printf("spawn stat collector")

	count_spawned := 0
	count_closed := 0

	for {
		gg := <-ch1

		if gg > 0 {
			count_spawned = count_spawned + gg
		} else {
			count_closed = count_closed + gg*-1
		}

		log.Printf("stat collector: total count ws connections: %d", count_spawned-count_closed)
	}
}

var chan_update_info = make(chan updateInfo, 1000)

func collector_updateInfo(cc chan updateInfo) {
	log.Printf("spawn update info collector")

	_last_opid := 0
	_info_2_uid := make(map[string]updateInfo)

	for {
		gg := <-cc

		if gg.opid == -1 {
			// это команда на запрос last_opid
			gg1 := updateInfo{"", _last_opid, 0}
			cc <- gg1
			//			log.Printf("collector:ret gg1=%s", gg1)
		} else if gg.update_counter == -1 {
			// это команда на запрос udate_counter по uid
			gg1 := _info_2_uid[gg.uid]

			cc <- gg1
			if gg1.update_counter > 0 {
				log.Printf("collector:ret: uid=%s opid=%d update_counter=%d", gg1.uid, gg1.opid, gg1.update_counter)
			}
		} else {
			_info_2_uid[gg.uid] = gg
			if _last_opid < gg.opid {
				_last_opid = gg.opid
				log.Printf("collector:last_opid=%d", _last_opid)
			}
			log.Printf("collector:update info: uid=%s opid=%d update_counter=%d", gg.uid, gg.opid, gg.update_counter)
		}
	}
}

func main() {

	log.SetFlags(log.LstdFlags | log.Lmicroseconds)

	go collector_updateInfo(chan_update_info)
	go collector_stat(ch1)

	http.HandleFunc("/ccus", wsHandler)

	if err := http.ListenAndServe(ADDR, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}
