#!/bin/sh

git clone --mirror git@github.com:semantic-machines/veda-bin.git
wget https://repo1.maven.org/maven2/com/madgag/bfg/1.13.0/bfg-1.13.0.jar
java -jar bfg-1.13.0.jar --delete-folders $TRAVIS_BUILD_NUMBER --no-blob-protection veda-bin.git
cd veda-bin.git
git reflog expire --expire=now --all && git gc --prune=now --aggressive
git push
cd ..
rm -rf veda-bin.git
