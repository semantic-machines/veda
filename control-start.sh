#!/bin/bash

ulimit -c unlimited
mkdir logs
mkdir data
mkdir data/tarantool

#tarantool ./source/init_tarantool.lua 2>./logs/tarantool-stderr.log  >./logs/tarantool-stdout.log &

/sbin/start-stop-daemon --start --verbose --chdir $PWD --make-pidfile --pidfile $PWD/.veda-pid --background --startas /bin/bash -- -c "exec ./veda no-watchdog >> $PWD/logs/veda-console.log 2>&1"

exit 0