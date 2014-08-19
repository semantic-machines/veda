#!/bin/sh
rm *.log
rm veda.app
dub build --build=release --config=trace-app
./veda.app