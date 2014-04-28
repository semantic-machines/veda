#!/bin/sh
rm *.log
rm veda.app
./dub build --build=release
./veda.app