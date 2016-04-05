#!/bin/sh
rm *.log
ln -s veda veda-fts-worker
ln -s veda veda-js-worker
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh
# build veda-server
rm veda-server
rm dub.selections.json
rm dub.json
cp source/veda-server.dub.json dub.json 
dub build veda-server --build=release
rm dub.selections.json
rm dub.json
# build veda-bootstrap
rm veda-bootstrap
rm dub.selections.json
rm dub.json
cp source/veda-bootstrap.dub.json dub.json 
dub build veda --build=release
rm dub.selections.json
rm dub.json
