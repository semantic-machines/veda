#!/bin/bash
username="admin"
password="4d1af0e10dab5fe07ae8d23bad5650b46804fb110cfb92f119213bc86aa03d34"

individual='{"@":"cfg:'$1'","rdf:type":[{"type":"Uri","data":"rdfs:Resource"}]}'
echo "individual = $individual"

dir=$(dirname "$0")
echo "dir = $dir"

cfg_dir=$(dirname "$dir")
echo "cfg_dir = $cfg_dir"

port=$(grep -oP '^\s*http_port\s*=\s*\K\d+' $cfg_dir/veda.properties)
echo "port = $port"

auth_request="curl -s -X GET http://localhost:$port/authenticate?login=$username&password=$password"
echo "auth_request = $auth_request"

auth_result=$($auth_request)
echo "auth_result = $auth_result"

ticket="$(echo $auth_result | grep -oE '"id":"[^"]*"' | grep -oE '[^"]{3,}')"
echo "ticket = $ticket"

payload='{"ticket":"'$ticket'","individual":'$individual',"prepare_events":true,"event_id":"","transaction_id":""}'
echo "payload = $payload"

put_request="curl -s -X PUT -H 'Content-Type: application/json' http://localhost:$port/put_individual -d '$payload'"
echo "put_request = $put_request"

put_result=$(eval $put_request)
echo "put_result = $put_result"
