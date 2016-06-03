#!/bin/sh
rm *.log
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh

echo *** build veda-bootstrap ***
cd source/dub/bootstrap
rm veda-bootstrap
rm dub.selections.json
dub build veda --build=release
cd ../../..
mv source/dub/bootstrap/veda veda

echo *** build veda-server ***
cd source/dub/server
rm veda-server
rm dub.selections.json
dub build veda-server --build=debug --config=trace-app
cd ../../..
mv source/dub/server/veda-server veda-server

echo *** build veda-fanout ***
cd source/dub/fanout
rm veda-fanout
rm dub.selections.json
dub build veda-fanout --build=debug --config=trace-app
cd ../../..
mv source/dub/fanout/veda-fanout veda-fanout

echo *** build veda-scripts ***
cd source/dub/scripts
rm veda-scripts
rm dub.selections.json
dub build veda-scripts --build=debug --config=trace-app
cd ../../..
mv source/dub/scripts/veda-scripts veda-scripts
