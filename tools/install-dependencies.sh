#!/bin/bash
DIST_LIB_PATH=../dist/lib

#libnanomsg
sudo cp $DIST_LIB_PATH/libnanomsg.so.5.1.0 /usr/local/lib
sudo cp -d $DIST_LIB_PATH/libnanomsg.so.5 /usr/local/lib
sudo cp -d $DIST_LIB_PATH/libnanomsg.so /usr/local/lib
sudo chmod 644 /usr/local/lib/libnanomsg.so.5.1.0
sudo chmod 644 /usr/local/lib/libnanomsg.so.5
sudo chmod 644 /usr/local/lib/libnanomsg.so

#libraptor2
sudo cp $DIST_LIB_PATH/libraptor2.so.0.0.0 /usr/local/lib
sudo cp -d $DIST_LIB_PATH/libraptor2.so.0 /usr/local/lib
sudo cp -d $DIST_LIB_PATH/libraptor2.so /usr/local/lib
sudo chmod 644 /usr/local/lib/libraptor2.so.0.0.0
sudo chmod 644 /usr/local/lib/libraptor2.so.0
sudo chmod 644 /usr/local/lib/libraptor2.so

#libtarantool.so.2.0
sudo cp $DIST_LIB_PATH/libtarantool.so.2.0.0 /usr/local/lib
sudo cp $DIST_LIB_PATH/libtarantool.so.2.0 /usr/local/lib
sudo cp $DIST_LIB_PATH/libtarantool.so /usr/local/lib
sudo chmod 644 /usr/local/lib/libtarantool.so.2.0.0
sudo chmod 644 /usr/local/lib/libtarantool.so.2.0
sudo chmod 644 /usr/local/lib/libtarantool.so

#libauthorization
sudo cp $DIST_LIB_PATH/libauthorization.so /usr/local/lib

./install-repo-libs.sh

sudo ldconfig
