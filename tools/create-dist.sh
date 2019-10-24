#!/bin/bash
DIST_PATH=../dist
mkdir $DIST_PATH
mkdir $DIST_PATH/lib
mkdir $DIST_PATH/tools

cp /usr/local/lib/libnanomsg.so.5.1.0 $DIST_PATH/lib
cp -d /usr/local/lib/libnanomsg.so.5 $DIST_PATH/lib
cp -d /usr/local/lib/libnanomsg.so $DIST_PATH/lib

cp /usr/local/lib/libraptor2.so.0.0.0 $DIST_PATH/lib
cp -d /usr/local/lib/libraptor2.so.0 $DIST_PATH/lib
cp -d /usr/local/lib/libraptor2.so $DIST_PATH/lib

cp /usr/local/lib/libtarantool.so.2.0.0 $DIST_PATH/lib
cp -d /usr/local/lib/libtarantool.so.2.0 $DIST_PATH/lib
cp -d /usr/local/lib/libtarantool.so $DIST_PATH/lib

cp ../libauthorization.so $DIST_PATH/lib

cp -r ../{cron,ontology,public,doc} $DIST_PATH

cp ../tools/control-backup.sh $DIST_PATH/tools
cp ../tools/install-dependencies.sh $DIST_PATH/tools
cp ../tools/install-repo-libs.sh $DIST_PATH/tools

cp ../{control-start.sh,control-stop.sh,veda,veda-*,veda.properties} $DIST_PATH
