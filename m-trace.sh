#!/bin/sh
rm *.log
rm veda-trace
rm -r .dub
rm dub.selections.json
#dub build --build=debug --config=trace-app
#./veda
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
dub build --build=debug --config=trace-app
mv veda veda-trace