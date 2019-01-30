#!/bin/sh
individual='{"@":"cfg:weekly", "rdf:type":[{"type":"Uri","data":"rdfs:Resource"}]}'
username="admin"
password=""

request_auth="curl -s -X GET http://localhost:8080/authenticate?login=$username&password=$password"
response_auth=$($request_auth)
ticket="$(echo $response_auth | grep -oE '"id":"[^"]*"' | grep -oE '[^"]{3,}')"
put_data='{"ticket":"'$ticket'","individual":'$individual',"prepare_events":true,"event_id":"","transaction_id":""}'
request_put="curl -s -X PUT -H 'Content-Type: application/json' http://localhost:8080/put_individual -d '$put_data'"
eval $request_put
