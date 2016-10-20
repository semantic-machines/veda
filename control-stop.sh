#!/bin/bash

start-stop-daemon -Kp $PWD/veda-pid $PWD/veda
killall -9 veda
killall -9 veda-webserver
killall -9 veda-server
killall -9 veda-scripts
killall -9 veda-ltr-scripts
killall -9 veda-ft-indexer
killall -9 veda-fanout-email
killall -9 veda-fanout-sql
killall -9 veda-ttlreader

rm data/module-info/*.lock
rm data/queue/*.lock