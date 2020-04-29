#!/bin/sh

./git-init.sh
git clone git@github.com:semantic-machines/veda-bin.git
mkdir ./veda-bin/$TRAVIS_BUILD_NUMBER/
mv veda veda-* ./veda-bin/$TRAVIS_BUILD_NUMBER/
cd veda-bin
git add .
git commit -a -q --no-edit -m "Build $TRAVIS_BUILD_NUMBER"
git push
cd ..
rm -rf ./veda-bin
