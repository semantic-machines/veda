#!/bin/sh
rm *.log
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh

rm veda
rm veda-server
rm veda-fanout
rm veda-scripts
rm veda-search
rm veda-ltr-scripts

echo *** build veda-bootstrap ***
rm veda-bootstrap
cd source/dub/bootstrap
rm dub.selections.json
dub build veda --build=release
cd ../../..
mv source/dub/bootstrap/veda veda
rm -r source/dub/bootstrap/.dub

echo *** build veda-server ***
rm veda-server
cd source/dub/server
rm dub.selections.json
dub build veda-server --build=release
cd ../../..
mv source/dub/server/veda-server veda-server
rm -r source/dub/server/.dub

echo *** build veda-fanout ***
rm veda-fanout
cd source/dub/fanout
rm dub.selections.json
dub build veda-fanout --build=debug
cd ../../..
mv source/dub/fanout/veda-fanout veda-fanout
rm -r source/dub/fanout/.dub

echo *** build veda-scripts ***
rm veda-scripts
cd source/dub/scripts
rm dub.selections.json
dub build veda-scripts --build=debug
cd ../../..
mv source/dub/scripts/veda-scripts veda-scripts
rm -r source/dub/scripts/.dub

echo *** build veda-ft-indexer ***
rm veda-ft-indexer
cd source/dub/ft-indexer
rm dub.selections.json
dub build veda-ft-indexer --build=debug
cd ../../..
mv source/dub/ft-indexer/veda-ft-indexer veda-ft-indexer
rm -r source/dub/ft-indexer/.dub

echo *** build veda-ltr-scripts ***
rm veda-ltr-scripts
cd source/dub/ltr-scripts
rm dub.selections.json
dub build veda-ltr-scripts --build=debug
cd ../../..
mv source/dub/ltr-scripts/veda-ltr-scripts veda-ltr-scripts
rm -r source/dub/ltr-scripts/.dub
