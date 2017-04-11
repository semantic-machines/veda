package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"
)

var httpClient *http.Client

func createRequest() (*http.Request, error) {
	httpClient = &http.Client{}
	req, err := http.NewRequest("GET", "http://127.0.0.1:8088/debug/vars", nil)
	if err != nil {
		return nil, err
	}
	req.Header.Add("Accept", "application/json")

	return req, nil
}

func main() {
	fmt.Println("CHART netdata.plugin_vedad_count_requests '' 'Veda count_requests' 'count' veda.d " +
		" '' area 1000 5")
	fmt.Println("DIMENSION count_requests 'count requests' absolute 1 1" +
		" '' area 1000 5")

	fmt.Println("CHART netdata.plugin_vedad_count_updates '' 'Veda count_updates' 'count' veda.d " +
		" '' area 1000 5")
	fmt.Println("DIMENSION count_updates 'count updates' absolute 1 1" +
		" '' area 1000 5")

	fmt.Println("CHART netdata.plugin_vedad_count_ws_sessions '' 'Veda count_ws_sessions' 'count' " +
		"veda.d '' area 1000 5")
	fmt.Println("DIMENSION count_ws_sessions 'count ws sessions' absolute 1 1" +
		" '' area 1000 5")

	fmt.Println("CHART netdata.plugin_vedad_dt_count_updates '' 'Veda dt_count_updates' 'count' " +
		"veda.d '' area 1000 5")
	fmt.Println("DIMENSION dt_count_updates 'dt count updates' absolute 1 1")

	req, err := createRequest()
	if err != nil {
		log.Println(err)
	}
	vedaData := make(map[string]interface{})
	for {
		respStream, err := httpClient.Do(req)
		if err != nil {
			// log.Println("Failed to do request: ", err)
			respStream.Body.Close()
			time.Sleep(5000 * time.Millisecond)
			req, err = createRequest()
			if err != nil {
				log.Println(err)
			}

			continue
		}

		decoder := json.NewDecoder(respStream.Body)
		err = decoder.Decode(&vedaData)
		if err != nil {
			// log.Println("Error on decoding json: ", err)
			respStream.Body.Close()
			time.Sleep(5000 * time.Millisecond)
			req, err = createRequest()
			if err != nil {
				log.Println(err)
			}

			continue
		}

		fmt.Println("BEGIN netdata.plugin_vedad_count_requests")
		fmt.Printf("SET count_requests=%v\n", vedaData["count_requests"])
		fmt.Println("END")

		fmt.Println("BEGIN netdata.plugin_vedad_count_updates")
		fmt.Printf("SET count_updates=%v\n", vedaData["count_updates"])
		fmt.Println("END")

		fmt.Println("BEGIN netdata.plugin_vedad_count_ws_sessions")
		fmt.Printf("SET count_ws_sessions=%v\n", vedaData["count_ws_sessions"])
		fmt.Println("END")

		fmt.Println("BEGIN netdata.plugin_vedad_dt_count_updates")
		fmt.Printf("SET dt_count_updates=%v\n", vedaData["dt_count_updates"])
		fmt.Println("END")

		respStream.Body.Close()
		time.Sleep(5000 * time.Millisecond)
	}

	/*	                    sys.stdout.write(
	    "CHART netdata.plugin_pythond_" +
	    chart +
	    " '' 'Execution time for " +
	    chart +
	    " plugin' 'milliseconds / run' python.d netdata.plugin_python area 145000 " +
	    str(job.timetable['freq']) +
	    '\n')
	sys.stdout.write("DIMENSION run_time 'run time' absolute 1 1\n\n")*/
}
