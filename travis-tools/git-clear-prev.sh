#!/bin/bash

git clone --mirror git@github.com:semantic-machines/veda-bin.git
wget https://repo1.maven.org/maven2/com/madgag/bfg/1.13.0/bfg-1.13.0.jar

for folder in ./veda-bin.git/*; do
  name=$(basename -- "$folder")
  if [ -d "$folder" ] && [ "$name" != "$TRAVIS_BUILD_NUMBER" ] && [[ "$name" =~ ^[0-9]+$ ]]; then
    java -jar bfg-1.13.0.jar --delete-folders $folder --no-blob-protection veda-bin.git
  fi
done

cd veda-bin.git
git reflog expire --expire=now --all && git gc --prune=now --aggressive
git push
cd ..
rm -rf veda-bin.git
