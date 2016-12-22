package main

import (
	"cbor"
	"github.com/gorilla/websocket"
	"log"
	"net/http"
	"strings"
	"time"
	//"strconv"
	"expvar"
	"runtime"
)

const (
	WS_LISTEN_ADDR string = ":8088"
)

type updateInfo struct {
	uid            string
	opid           int
	update_counter int
	cc_out         chan updateInfo
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
	ws, err := websocket.Upgrade(w, r, nil, 1024, 1024)
	if _, ok := err.(websocket.HandshakeError); ok {
		http.Error(w, "wsHandler: not a websocket handshake", 400)
		return
	} else if err != nil {
		log.Printf("wsHandler, err=%s", err)
		return
	}
	NewCcusConn(ws, ch_update_info_in)
}

var ch_ws_counter = make(chan int, 1000)

func goroutines() interface{} {
	return runtime.NumGoroutine()
}

//  collector_stat - routine that collects data on the number of current WS connections.
//  send to [ch1 chan int], +1 or -1
func collector_stat(ch1 chan int) {
	log.Printf("spawn stat collector")

	count_spawned := 0
	count_closed := 0

	for {
		gg := <-ch1
		//log.Printf("stat collector: (%d)", gg)

		if gg > 0 {
			count_spawned = count_spawned + gg
		} else {
			count_closed = count_closed - gg
		}

		g_count_ws_sessions.Set(int64(count_spawned - count_closed))

		log.Printf("stat collector: total count ws connections: %d (%d)", count_spawned-count_closed, gg)
	}
}

var ch_update_info_in = make(chan updateInfo, 1000)

//  queue_reader - routine that read queue, and transmits information about the update to collector_updateInfo routine
func queue_reader(ch_collector_update chan updateInfo) {

	time.Sleep(1000 * time.Millisecond)

	main_queue_name := "individuals-flow"
	var main_queue *Queue
	var main_cs *Consumer

	main_queue = NewQueue(main_queue_name, R)
	main_queue.open(CURRENT)

	main_cs = NewConsumer(main_queue, "CCUS")
	main_cs.open()

	data := ""
	count := 0

	for {
		time.Sleep(300 * time.Millisecond)

		main_queue.reopen_reader()

		t0 := time.Now()
		count_0 := count

		//log.Printf("@start prepare batch, count=%d", count)
		for true {
			data = main_cs.pop()
			if data == "" {
				break
			}
			//			log.Printf("@1 data=[%s].length=%d", data, len (data))
			ring := cbor.NewDecoder(strings.NewReader(data))
			var cborObject interface{}
			err := ring.Decode(&cborObject)
			if err != nil {
				log.Fatalf("error decoding cbor: %v", err)
				continue
			}

			var individual *Individual = NewIndividual()

			cbor2individual(individual, cborObject)

			//log.Printf("@2 individual=[%v]", individual)
			uri := individual.getFirstResource("uri")
			u_count, ok1 := individual.getFirstInt("u_count")
			op_id, ok2 := individual.getFirstInt("op_id")

			if ok1 == true && ok2 == true {
				//log.Printf("@3 uri=[%s], u_count=[%d], op_id=[%d]", uri.data.(string), u_count, op_id)

				new_info := updateInfo{uri.data.(string), op_id, u_count, nil}
				ch_collector_update <- new_info
			}

			main_cs.commit_and_next(false)
			count++
		}
		count_1 := count

		delta_count := count_1 - count_0

		if delta_count > 0 {
			t1 := time.Now()
			delta := t1.Sub(t0).Seconds()
			log.Printf("processed batch, count=%d, total time=%v, cps=%.2f", delta_count, t1.Sub(t0), float64(delta_count)/delta)

			main_cs.sync()
		}
		//ch_collector_update
	}
}

func collector_updateInfo(ch_collector_update chan updateInfo) {
	log.Printf("spawn: update info collector")
	_last_opid := 0
	_info_2_uid := make(map[string]updateInfo)
	count_updates := 0

	for {
		arg := <-ch_collector_update

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
			count_updates = count_updates + 1
			if _last_opid < arg.opid {
				_last_opid = arg.opid
				//log.Printf("collector:set last_opid=%d", _last_opid)
			}
			//				log.Printf("collector:update info: uid=%s opid=%d update_counter=%d", arg.uid, arg.opid, arg.update_counter)
			g_count_updates.Set(int64(count_updates))

			if count_updates%1000 == 0 {
				//log.Printf("collector:update info: uid=%s opid=%d update_counter=%d, total count=%d", arg.uid, arg.opid, arg.update_counter, count_updates)
				log.Printf("collector:update info: total update count=%d", count_updates)
			}
		}
	}
}

var g_count_updates *expvar.Int
var g_count_ws_sessions *expvar.Int

func main() {

	log.SetFlags(log.LstdFlags | log.Lmicroseconds)

	go collector_updateInfo(ch_update_info_in)
	go collector_stat(ch_ws_counter)
	go queue_reader(ch_update_info_in)

	http.HandleFunc("/ccus", wsHandler)

	g_count_updates = expvar.NewInt("count_updates")
	g_count_ws_sessions = expvar.NewInt("g_count_ws_sessions")

	if err := http.ListenAndServe(WS_LISTEN_ADDR, nil); err != nil {
		log.Fatal("listen and serve:", err)
	}
}
