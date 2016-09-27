#!/bin/sh
rm *.log
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh

./build-component.sh veda-bootstrap bootstrap
rm veda
rename "s/veda-bootstrap/veda/g" *
./build-component.sh veda-server server
./build-component.sh veda-fanout fanout
./build-component.sh veda-scripts scripts
./build-component.sh veda-ft-indexer ft-indexer
./build-component.sh veda-ltr-scripts ltr-scripts
./build-component.sh veda-acl-preparer acl-preparer
./build-component.sh veda-ttlreader ttlreader
