#!/bin/sh
rm *.log
rm veda.app
dub build --build=release --config=app
./veda.app