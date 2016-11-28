#!/bin/sh
echo ********* build $1 **********
rm $1
cd source/dub/$2
rm dub.selections.json
dub build $1 --build=release
cd ../../..
mv source/dub/$2/$1 $1
rm -r source/dub/$2/.dub

