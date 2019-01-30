#!/bin/bash
username="admin"
password="4d1af0e10dab5fe07ae8d23bad5650b46804fb110cfb92f119213bc86aa03d34"

individual='{"@":"cfg:'$1'","rdf:type":[{"type":"Uri","data":"rdfs:Resource"}]}'
dir=$(dirname "$0")
cfg_dir=$(dirname "$dir")
port=$(grep -oP '^\s*http_port\s*=\s*\K\d+' $cfg_dir/veda.properties)

echo "individual = $individual"
echo "dir = $dir"
echo "cfg_dir = $cfg_dir"
echo "port = $port"

request_auth="curl -s -X GET http://localhost:$port/authenticate?login=$username&password=$password"
response_auth=$($request_auth)
ticket="$(echo $response_auth | grep -oE '"id":"[^"]*"' | grep -oE '[^"]{3,}')"

echo $ticket

put_data='{"ticket":"'$ticket'","individual":'$individual',"prepare_events":true,"event_id":"","transaction_id":""}'
request_put="curl -s -X PUT -H 'Content-Type: application/json' http://localhost:$port/put_individual -d '$put_data'"
eval $request_put

echo
