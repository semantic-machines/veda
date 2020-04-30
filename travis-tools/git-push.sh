#!/bin/sh

echo $SSHKEY | base64 -d > id_rsa
echo $SSHKEYPUB | base64 -d > id_rsa.pub
chmod 700 id_rsa id_rsa.pub
mv -f id_rsa id_rsa.pub ~/.ssh
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
git config --global user.email "info@semantic-machines.com"
git config --global user.name "Info SM"
git config --global push.default simple

git clone git@github.com:semantic-machines/veda-bin.git
mkdir ./veda-bin/$TRAVIS_BUILD_NUMBER/
mv veda veda-* libauthorization.so ./veda-bin/$TRAVIS_BUILD_NUMBER/
cd veda-bin
git add .
git commit -a -q --no-edit -m "Build $TRAVIS_BUILD_NUMBER"
git push
cd ..
rm -rf ./veda-bin
