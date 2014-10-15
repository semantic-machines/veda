#!/bin/sh
rm *.log
rm veda.app
rm -r .dub
rm dub.selections.json
#dub build --build=debug --config=trace-app
#./veda.app
dub --config=trace-app