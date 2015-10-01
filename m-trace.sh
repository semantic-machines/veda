#!/bin/sh
rm *.log
rm veda
rm -r .dub
rm dub.selections.json
#dub build --build=debug --config=trace-app
#./veda
dub --config=trace-app