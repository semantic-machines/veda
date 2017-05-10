#!/bin/bash

start-stop-daemon -Kp $PWD/veda-pid $PWD/veda
killall -9 veda
killall -9 veda-ccus
killall -9 veda-fanout-email
killall -9 veda-fanout-sql
killall -9 veda-fanout-sql-lp
killall -9 veda-ft-indexer
killall -9 veda-ltr-scripts
killall -9 veda-scripts-lp
killall -9 veda-scripts-main
killall -9 veda-mstorage
killall -9 veda-ttlreader
killall -9 veda-webserver

rm data/module-info/*.lock
rm data/queue/*.lock
rm data/uris/*.lock
