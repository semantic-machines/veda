#!/bin/sh

mv ./veda-bin/$TRAVIS_BUILD_NUMBER/* ./
sudo cp libauthorization.so /usr/local/lib
sudo ldconfig
rm -rf ./veda-bin

if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi

if [ ! -f ./ontology/system-accounts.ttl ]
then
  cp ./ontology/system-accounts.ttl.cfg ./ontology/system-accounts.ttl
fi

./tools/update-version-ttl.sh
