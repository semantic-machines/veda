#!/bin/sh
rm *.log
rm veda.app
rm -r .dub
dub build --build=release --config=trace-app
./veda.app