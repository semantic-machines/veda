#!/bin/sh
rm *.log
rm veda
rm dub.selections.json
#dub build --build=debug --config=app
dub --build=debug
#./veda
