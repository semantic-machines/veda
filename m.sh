#!/bin/sh
rm *.log
rm veda
rm dub.selections.json
#dub build --build=debug --config=app
ln -s veda veda-fts-worker
ln -s veda veda-js-worker
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh
dub build --build=release
#./veda
