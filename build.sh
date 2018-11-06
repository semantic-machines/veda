BUILD_PATH=$PWD

./stop

#!/bin/sh
rm *.log
rm ./logs/*.log
rm -r ./logs

if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi
./tools/update-version-ttl.sh

    cd source/authorization
    cargo build --release
    cd $BUILD_PATH
    sudo cp ./source/lib64/libauthorization.so $PWD
    sudo cp $PWD/libauthorization.so /usr/local/lib
    sudo ldconfig

if [ -z $1 ] || [ $1 == "ccus" ] || [ $1 == "veda-ccus" ] ; then

    echo make start VEDA-CCUS
    if [ -z $GOROOT ]; then
	export GOROOT=/usr/local/go
    else 
	echo "var GOROOT is set to '$GOROOT'"; 
    fi

    export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
    export GOPATH=$HOME/go
    rm ./veda-ccus
    go build -o veda-ccus  source/ccus/src/ccus/cbor_tags.go source/ccus/src/ccus/binobj2map.go source/ccus/src/ccus/tools.go source/ccus/src/ccus/queue.go source/ccus/src/ccus/ccus.go source/ccus/src/ccus/preparer.go
    echo make end VEDA-CCUS
fi

if [ -z $1 ] || [ $1 == "bootstrap" ] || [ $1 == "veda" ] ; then

    ./tools/build-component.sh veda-bootstrap bootstrap
    rm veda
    rename "s/veda-bootstrap/veda/g" *
fi

if [ -z $1 ] || [ $1 == "mstorage" ] || [ $1 == "veda-mstorage" ] ; then
    ./tools/build-component.sh veda-mstorage mstorage
fi

if [ -z $1 ] || [ $1 == "lmdb-srv" ] || [ $1 == "veda-lmdb-srv" ] ; then
    ./tools/build-component.sh veda-lmdb-srv lmdb-srv
fi

if [ -z $1 ] || [ $1 == "authorization" ] || [ $1 == "veda-authorization" ] ; then
    ./tools/build-component.sh veda-authorization authorization
fi

if [ -z $1 ] || [ $1 == "fanout-email" ] || [ $1 == "veda-fanout-email" ] ; then
    ./tools/build-component.sh veda-fanout-email fanout-email
fi

if [ -z $1 ] || [ $1 == "fanout-sql-np" ] || [ $1 == "veda-fanout-sql-np" ] ; then
    ./tools/build-component.sh veda-fanout-sql-np fanout-sql-np
    ./tools/build-component.sh veda-fanout-sql-lp fanout-sql-lp
fi

if [ -z $1 ] || [ $1 == "scripts" ] || [ $1 == "veda-scripts" ] ; then
    ./tools/build-component.sh veda-scripts scripts
    rm veda-scripts-main
    rm veda-scripts-lp
    rm veda-ltr-scripts
fi

if [ -z $1 ] || [ $1 == "ft-indexer" ] || [ $1 == "veda-ft-indexer" ] ; then
    ./tools/build-component.sh veda-ft-indexer ft-indexer
fi

if [ -z $1 ] || [ $1 == "ttlreader" ] || [ $1 == "veda-ttlreader" ] ; then
    ./tools/build-component.sh veda-ttlreader ttlreader
fi

if [ -z $1 ] || [ $1 == "ft-query" ] || [ $1 == "veda-ft-query" ] ; then
    ./tools/build-component.sh veda-ft-query ft-query
fi

if [ -z $1 ] || [ $1 == "input-queue" ] || [ $1 == "veda-input-queue" ] ; then
    ./tools/build-component.sh veda-input-queue input-queue
fi

if [ -z $1 ] || [ $1 == "gowebserver" ] || [ $1 == "veda-gowebserver" ]; then
    cd source/veda-gowebserver
    go build
    cd $BUILD_PATH
    cp source/veda-gowebserver/veda-gowebserver ./veda-gowebserver
fi

#if [ -z $1 ] || [ $1 == "db_handler" ] ; then
#  cd source/rust_db_handler/db_handler
#  cargo build --release
#  cd ../../..
#fi
