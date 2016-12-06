#sudo ifdown -a

#!/bin/sh
rm *.log
rm ./logs/*.log
if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./update-version-ttl.sh

if [ -z $1 ] || [ $1 == "ccus" ] ; then

    echo make start VEDA-CCUS
    if [ -z $GOROOT ]; then
	export GOROOT=/usr/local/go
    else 
	echo "var GOROOT is set to '$GOROOT'"; 
    fi

    export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
    export GOPATH=$HOME/go
    rm ./veda-ccus
    go build -o veda-ccus source/ccus/src/ccus/ccus.go source/ccus/src/ccus/conn.go
    echo make end VEDA-CCUS
fi

if [ -z $1 ] || [ $1 == "bootstrap" ] ; then

    ./build-component.sh veda-bootstrap bootstrap
    rm veda
    rename "s/veda-bootstrap/veda/g" *
fi

if [ -z $1 ] || [ $1 == "webserver" ] ; then
    ./build-component.sh veda-webserver webserver
fi

if [ -z $1 ] || [ $1 == "server" ] ; then
    ./build-component.sh veda-server server
fi

if [ -z $1 ] || [ $1 == "fanout-email" ] ; then
    ./build-component.sh veda-fanout-email fanout-email
fi

if [ -z $1 ] || [ $1 == "fanout-sql" ] ; then
    ./build-component.sh veda-fanout-sql fanout-sql
fi

if [ -z $1 ] || [ $1 == "scripts-main" ] ; then
    ./build-component.sh veda-scripts-main scripts-main
    rm veda-scripts
fi

if [ -z $1 ] || [ $1 == "scripts-lp" ] ; then
    ./build-component.sh veda-scripts-lp scripts-lp
fi

if [ -z $1 ] || [ $1 == "ft-indexer" ] ; then
    ./build-component.sh veda-ft-indexer ft-indexer
fi

if [ -z $1 ] || [ $1 == "ltr-scripts" ] ; then
    ./build-component.sh veda-ltr-scripts ltr-scripts
fi

if [ -z $1 ] || [ $1 == "ttlreader" ] ; then
    ./build-component.sh veda-ttlreader ttlreader
fi

#sudo ifup -a
