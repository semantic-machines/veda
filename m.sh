#!/bin/sh
rm *.log
rm veda.app
rm dub.selections.json
#dub build --build=debug --config=app
dub
#./veda.app
