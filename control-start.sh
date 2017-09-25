#!/bin/bash

ulimit -c unlimited
mkdir logs
/sbin/start-stop-daemon --start --verbose --chdir $PWD --make-pidfile --pidfile $PWD/.veda-pid --background --startas /bin/bash -- -c "exec ./veda no-watchdog >> $PWD/logs/veda-console.log 2>&1"