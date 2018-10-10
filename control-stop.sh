#!/bin/bash

TIMESTAMP=`date +%Y-%m-%d_%H_%M`

mkdir ./logs/$TIMESTAMP
cp ./logs/*-stderr.log ./logs/$TIMESTAMP

#tarantoolctl stop init_tarantool.lua
#pkill tarantool

start-stop-daemon -Kp $PWD/.pids/veda-pid $PWD/veda

target=".pids/"
let count=0
for f in "$target"/*
do
    pid=`cat $target/$(basename $f)`
    echo kill $pid
    kill -kill $pid
    let count=count+1
done
echo ""
echo "Count: $count"

rm -f -r .pids
rm -f data/module-info/*.lock
rm -f data/queue/*.lock
rm -f data/uris/*.lock

exit 0