#!/bin/sh
rm *.log
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh
# build veda-bootstrap
rm veda-bootstrap
rm dub.selections.json
cp source/veda-bootstrap.dub.json dub.json 
dub build veda --build=debug --config=trace-app
# build veda-server
rm veda-server
rm dub.selections.json
cp source/veda-server.dub.json dub.json 
dub build veda-server --build=debug --config=trace-app
rm dub.selections.json

