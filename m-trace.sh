#!/bin/sh
rm *.log
rm veda.app
rm -r .dub
rm dub.selections.json
dub build --build=release --config=trace-app
./veda.app