#!/bin/bash

start-stop-daemon -Kp $PWD/veda-pid $PWD/veda
killall -9 veda
killall -9 veda-server
killall -9 veda-scripts
killall -9 veda-ltr-scripts
killall -9 veda-ft-indexer
killall -9 veda-fanout
killall -9 veda-ttlreader
