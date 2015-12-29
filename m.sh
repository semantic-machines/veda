#!/bin/sh
rm *.log
rm veda
rm dub.selections.json
#dub build --build=debug --config=app
ln -s veda veda-fts-worker
ln -s veda veda-js-worker
dub --build=release
#./veda
