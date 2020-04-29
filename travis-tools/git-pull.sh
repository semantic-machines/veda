#!/bin/sh

./git-init.sh
git clone git@github.com:semantic-machines/veda-bin
mv ./veda-bin/$TRAVIS_BUILD_NUMBER/* ./
rm -rf ./veda-bin
