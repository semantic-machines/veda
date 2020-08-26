#!/bin/bash

git clone --mirror git@github.com:semantic-machines/veda-bin.git
wget -nv https://repo1.maven.org/maven2/com/madgag/bfg/1.13.0/bfg-1.13.0.jar

todelete=""
for folder in ./veda-bin/*; do
  name=$(basename -- "$folder")
  echo "current file: $folder, basename: $name"
  if [ -d "$folder" ] && [ "$name" != "$TRAVIS_BUILD_NUMBER" ] && [[ "$name" =~ ^[0-9]+$ ]]; then
    if [ -z "$todelete" ]; then
      todelete="$name"
    else 
      todelete="$todelete,$name"
    fi
  fi
done

if [ -n "$todelete" ]; then
  echo "Delete folders: $todelete"
  java -jar bfg-1.13.0.jar --delete-folders "{$todelete}" --no-blob-protection veda-bin.git
  cd veda-bin.git
  git reflog expire --expire=now --all && git gc --prune=now --aggressive
  git push
fi
