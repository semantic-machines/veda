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
	NewCcusConn(ws, cc)
}

var cc = make(chan updateInfo)

func collector_updateInfo(cc chan updateInfo) {
	log.Printf("spawn collector")

	_last_opid := 0
	_info_2_uid := make(map[string]updateInfo)

	for {
		gg := <-cc
		//log.Printf("collector: in gg=%s", gg)

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
				log.Printf("collector:ret gg1=%s", gg1)
			}
		} else {
			_info_2_uid[gg.uid] = gg
			if _last_opid < gg.opid {
				_last_opid = gg.opid
				log.Printf("collector:last_opid=%d", _last_opid)
			}
			log.Printf("collector:update info %s", gg)
		}
	}
}

func main() {

	go collector_updateInfo(cc)

	http.HandleFunc("/ccus", wsHandler)

	if err := http.ListenAndServe(ADDR, nil); err != nil {
		log.Fatal("ListenAndServe:", err)
	}
}
