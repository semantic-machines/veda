#!/bin/bash

if pgrep -f -l -U $USER '[t]arantool init_tarantool.lua'; then
    echo
    echo Attention! Tarantool database process is already running, please stop it before starting Veda. Run script.
    echo $ ./control-stop.sh all
    echo to stop tarantool and try to start veda again
    echo
    exit 0
else
    echo
fi

export APPDIR=$PWD/bin
VEDA_ID=A1

mkdir .pids
mkdir logs
mkdir data
mkdir data/tarantool
mkdir data/oxigraph
mkdir data/xapian-info
mkdir data/acl-indexes

ulimit -c unlimited

# start oxigraph server
#/sbin/start-stop-daemon --start --verbose --chdir $PWD --make-pidfile --pidfile $PWD/./.pids/oxigraph-pid --background --startas /bin/bash -- -c "exec ./oxigraph serve --location ./data/oxigraph -b 127.0.0.1:7878 2>./logs/oxigraph-stderr.log >./logs/oxigraph-stdout.log 2>&1"

# start tarantool server
/sbin/start-stop-daemon --start --verbose --chdir $PWD --make-pidfile --pidfile $PWD/./.pids/tt-pid --background --startas /bin/bash -- -c "exec tarantool ./source-server/source/init_tarantool.lua 2>./logs/tarantool-stderr.log  >./logs/tarantool-stdout.log 2>&1"

sleep 3

# Установка переменной LD_LIBRARY_PATH
export LD_LIBRARY_PATH=./bin/lib

# Устанавливаем путь для core-файлов
export COREDUMP_DIR=$GITHUB_WORKSPACE/corefiles
mkdir -p $COREDUMP_DIR

#export RUST_LOG="debug,actix_server=info,actix_web=info"
#export RUST_BACKTRACE=1
export AUTH_URL=tcp://127.0.0.1:8113
/sbin/start-stop-daemon --start --verbose --chdir $PWD --make-pidfile --pidfile $PWD/./.pids/veda-pid --background --startas /bin/bash -- -c "exec ./bin/veda --id=$VEDA_ID no-watchdog>> $PWD/logs/veda-console.log 2>&1"

exit 0
