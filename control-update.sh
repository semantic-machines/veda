#!/bin/bash

# Собирает и запускает

echo "= UPDATE = "
date
export PATH="$PATH:/sbin/"
if wget -q -O - "$@" https://api.travis-ci.org/repos/semantic-machines/veda/cc.xml?branch=master | grep 'lastBuildStatus="Success"'; then
    echo "=== Update project ==="
    git pull

    echo "=== Stop daemon ==="
    ./control-stop.sh

    echo "=== Generate JSduck documentation ==="
    ./jsduck.sh

    echo "=== Build project ==="
    ./build.sh

    echo "=== Start daemon =="
    ./control-start.sh
else
    echo "=== Current build is not stable. No update. For more information see: https://travis-ci.org/karpovr/veda/builds  ==="
fi