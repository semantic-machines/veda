#!/bin/sh
rm *.log
rm veda.app
rm dub.selections.json
dub build --build=release --config=app
./veda.app