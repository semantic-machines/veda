package main

import (
	"cbor"
	"github.com/gorilla/websocket"
	"log"
	"net"
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

type ConnActivity uint8

const (
	SPAWN   ConnActivity = 1
	DROP    ConnActivity = 2
	REQUEST ConnActivity = 3
)

type infoConn struct {
	activity ConnActivity
	addr     net.Addr
}

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

var ch_ws_counter = make(chan infoConn, 1000)

func goroutines() interface{} {
	return runtime.NumGoroutine()
}

//  collector_stat - routine that collects data on the number of current WS connections.
//  send to [ch1 chan int], +1 or -1
func collector_stat(ch1 chan infoConn) {
	log.Printf("spawn stat collector")

	var sessions map[net.Addr]int = make(map[net.Addr]int)
	var hosts map[string]int = make(map[string]int)

	count_spawned := 0
	count_closed := 0
	dt_count_request := 0
	t0 := time.Now()

	for {
		var gg infoConn

		select {

		case gg = <-ch1:
			var host, _, _ = net.SplitHostPort(gg.addr.String())
			if gg.activity == REQUEST {
				sessions[gg.addr] = sessions[gg.addr] + 1
				hosts[host] = hosts[host] + 1
				dt_count_request++
			} else if gg.activity == SPAWN {
				sessions[gg.addr] = 0
				hosts[host] = 0
				count_spawned++
				g_count_ws_sessions.Set(int64(len(sessions)))
				g_count_ws_hosts.Set(int64(len(hosts)))
				log.Printf("stat collector: ws spawn: total count ws connections: %d (%d)", len(sessions), count_spawned)
			} else if gg.activity == DROP {
				delete(sessions, gg.addr)
				delete(hosts, host)
				count_closed++
				g_count_ws_sessions.Set(int64(len(sessions)))
				g_count_ws_hosts.Set(int64(len(hosts)))
				log.Printf("stat collector: ws drop: total count ws connections: %d (%d)", len(sessions), count_spawned)
			}
			break

		default:
			time.Sleep(100 * time.Millisecond)
			t1 := time.Now()
			if t1.Sub(t0).Seconds() > 10 {
				dt_count_request = 0
				t0 = time.Now()
			}
			g_count_request.Set(int64(dt_count_request))
		}

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

	dt_count := 0

	dt_count_t0 := time.Now()
	dt_count_0 := count

	for {
		time.Sleep(300 * time.Millisecond)

		main_queue.reopen_reader()

		batch_t0 := time.Now()
		batch_count_0 := count

		//log.Printf("@start prepare batch, count=%d", count)
		for true {

			dt_count_t1 := time.Now()
			if dt_count_t1.Sub(dt_count_t0).Seconds() > 10 {
				dt_count = count - dt_count_0
				dt_count_0 = count
				dt_count_t0 = dt_count_t1
				g_dt_count_updates.Set(int64(dt_count))
			}

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

		g_count_updates.Set(int64(count))
		t1 := time.Now()

		batch_count_1 := count
		batch_dt_count := batch_count_1 - batch_count_0
		if batch_dt_count > 0 {
			delta_t := t1.Sub(batch_t0).Seconds()
			log.Printf("processed batch, count=%d, total time=%v, cps=%.2f", batch_dt_count, t1.Sub(batch_t0), float64(batch_dt_count)/delta_t)

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
			//	log.Printf("collector:update info: uid=%s opid=%d update_counter=%d", arg.uid, arg.opid, arg.update_counter)

			if count_updates%1000 == 0 {
				//log.Printf("collector:update info: uid=%s opid=%d update_counter=%d, total count=%d", arg.uid, arg.opid, arg.update_counter, count_updates)
				log.Printf("collector:update info: total update count=%d", count_updates)
			}
		}
	}
}

var g_dt_count_updates *expvar.Int
var g_count_updates *expvar.Int
var g_count_ws_sessions *expvar.Int
var g_count_ws_hosts *expvar.Int
var g_count_request *expvar.Int

func main() {
	log.SetFlags(log.LstdFlags | log.Lmicroseconds)

	go collector_updateInfo(ch_update_info_in)
	go collector_stat(ch_ws_counter)
	go queue_reader(ch_update_info_in)

	http.HandleFunc("/ccus", wsHandler)

	g_dt_count_updates = expvar.NewInt("dt_count_updates")
	g_count_updates = expvar.NewInt("count_updates")
	g_count_ws_sessions = expvar.NewInt("count_ws_sessions")
	g_count_ws_hosts = expvar.NewInt("count_ws_hosts")
	g_count_request = expvar.NewInt("count_request")

	log.Printf("Listen and serve: %s", WS_LISTEN_ADDR)
	if err := http.ListenAndServe(WS_LISTEN_ADDR, nil); err != nil {
		log.Fatal("ERR! listen and serve:", err)
	}
}
