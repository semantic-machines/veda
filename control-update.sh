echo "= UPDATE = "
date
if wget -q -O - "$@" https://api.travis-ci.org/repos/karpovr/veda/cc.xml?branch=master | grep 'lastBuildStatus="Success"'; then
    echo "=== Stop daemon ==="
    ./control-stop.sh

    echo "=== Remove old files ==="
    rm *.log
    rm veda
    rm dub.selections.json
    rm -r ~/.dub/cache

    echo "=== Update project ==="
    git pull

    echo "=== Generate JSduck documentation ==="
    ./jsduck.sh

    echo "=== Build dependencies ==="
    dub -v fetch vibe-d
    echo "=== Build project ==="
    dub -v build --build=debug

    echo "=== Start daemon =="
    ./control-start.sh
else
    echo "=== Current build is not stable. No update. For more information see: https://travis-ci.org/karpovr/veda/builds  ==="
fi