rustup override set 1.81

build_module () {
    BUILD_PATH=$PWD
    VEDA_BIN=$BUILD_PATH/bin
    module_name=$1

    echo $module_name
    rm ./$module_name

    cd source/$module_name
    cargo build --release
    status=$?
    if test $status -ne 0
    then
	exit $status;
    fi
    cd $BUILD_PATH
    cp $CARGO_TARGET_DIR/release/$module_name $VEDA_BIN
}

BUILD_PATH=$PWD

#!/bin/sh
rm *.log
rm ./logs/*.log
rm -r ./logs

mkdir ./bin
VEDA_BIN=$BUILD_PATH/bin

if [ ! -f ./ontology/config.ttl ]
then
  cp ./ontology/config.ttl.cfg ./ontology/config.ttl
fi

if [ ! -f ./ontology/system-accounts.ttl ]
then
  cp ./ontology/system-accounts.ttl.cfg ./ontology/system-accounts.ttl
fi

if [ ! -f ./ontology/test-data.ttl ]
then
  cp ./ontology/test-data.ttl.cfg ./ontology/test-data.ttl
fi

./tools/update-version-ttl.sh

export CARGO_TARGET_DIR=$HOME/target

git submodule init
git submodule update
cd ./source-server
./build.sh
cd $BUILD_PATH
cp ./source-server/bin/* ./bin


if [ $1 == "libvqueueinfo2c" ] || [ $1 == "all" ] ; then
    build_module "libvqueueinfo2c"

    cp $CARGO_TARGET_DIR/release/libvqueueinfo2c.so ./source/lib64/libvqueueinfo2c.so
    cp $CARGO_TARGET_DIR/release/libvqueueinfo2c.a ./source/lib64/libvqueueinfo2c.a
    cp $CARGO_TARGET_DIR/release/libvqueueinfo2c.so $VEDA_BIN/lib

fi

if [ $1 == "geo-indexer" ] || [ $1 == "veda-geo-indexer" ] || [ $1 == "all" ] ; then
    build_module "veda-geo-indexer"
fi

if [ -z $1 ] || [ $1 == "ro-storage" ] || [ $1 == "veda-ro-storage" ] || [ $1 == "basic" ] || [ $1 == "all" ] ; then
    build_module "veda-ro-storage"
fi

if [ $1 == "fanout-email" ] || [ $1 == "veda-fanout-email" ] || [ $1 == "all" ]; then
    build_module "veda-fanout-email"
fi

if [ $1 == "fanout-sql" ] || [ $1 == "veda-fanout-sql" ] || [ $1 == "all" ]; then
    build_module "veda-fanout-sql"
fi

if [ $1 == "search-index-tt" ] || [ $1 == "veda-search-index-tt" ] || [ $1 == "all" ]; then
    build_module "veda-search-index-tt"
fi

if [ $1 == "search-index-pt" ] || [ $1 == "veda-search-index-pt" ] || [ $1 == "all" ]; then
    build_module "veda-search-index-pt"
fi

if [ $1 == "search-query" ] || [ $1 == "veda-search-query" ] || [ $1 == "all" ]; then
    build_module "veda-search-query"
fi

if [ $1 == "tools" ] || [ $1 == "veda-tools" ] || [ $1 == "all" ]; then
    build_module "veda-tools"
fi

if [ -z $1 ] || [ $1 == "sparql-indexer" ] || [ $1 == "sparql-ft-indexer" ] || [ $1 == "basic" ] || [ $1 == "all" ]; then
    build_module "veda-sparql-indexer"
fi

if [ -z $1 ] || [ $1 == "queue2storage" ] || [ $1 == "veda-queue2storage" ] || [ $1 == "all" ]; then
    build_module "veda-queue2storage"
fi

if [ -z $1 ] || [ $1 == "web" ] || [ $1 == "public" ] || [ $1 == "all" ]; then
    echo BUILD PUBLIC

    cd source-web
    npm install
    npm run build
    cd $BUILD_PATH
fi
