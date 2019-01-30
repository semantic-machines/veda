#!/bin/bash
username="admin"
password=""

individual='{"@":"cfg:'$1'","rdf:type":[{"type":"Uri","data":"rdfs:Resource"}]}'
dir=$(dirname "$0")
cfg_dir=$(dirname "$dir")
port=$(grep -oP 'http_port=\K\d+' $cfg_dir/veda.properties)

request_auth="curl -s -X GET http://localhost:$port/authenticate?login=$username&password=$password"
response_auth=$($request_auth)
ticket="$(echo $response_auth | grep -oE '"id":"[^"]*"' | grep -oE '[^"]{3,}')"
put_data='{"ticket":"'$ticket'","individual":'$individual',"prepare_events":true,"event_id":"","transaction_id":""}'
request_put="curl -s -X PUT -H 'Content-Type: application/json' http://localhost:$port/put_individual -d '$put_data'"
eval $request_put
