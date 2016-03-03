# Собирает и запускает

echo "= UPDATE = "
date
export PATH="$PATH:/sbin/"
if wget -q -O - "$@" https://api.travis-ci.org/repos/semantic-machines/veda/cc.xml?branch=master | grep 'lastBuildStatus="Success"'; then
    echo "=== Update project ==="
    git pull

    echo "=== Stop daemon ==="
    ./control-stop.sh

    echo "=== Remove old files ==="
    rm *.log
    rm veda
    rm dub.selections.json
    rm -r ~/.dub/cache

    echo "=== Generate JSduck documentation ==="
    ./jsduck.sh

    echo "=== Build dependencies ==="
    dub -v fetch vibe-d --version=0.7.26
    echo "=== Build project ==="
    dub -v build --build=debug

    echo "=== Start daemon =="
    ./control-start.sh
else
    echo "=== Current build is not stable. No update. For more information see: https://travis-ci.org/karpovr/veda/builds  ==="
fi