#!/bin/sh
echo "\n**************** BUILD [$1] **************\n"
rm $1
cd source/dub/$2
rm dub.selections.json
#dub build $1 --build=release  --skip-registry=all
dub build $1 --build=release
cd ../../..
mv source/dub/$2/$1 $1
rm -r source/dub/$2/.dub

