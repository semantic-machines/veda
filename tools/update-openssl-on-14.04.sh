#!/bin/bash

sudo apt-get install make
wget https://www.openssl.org/source/openssl-1.0.2l.tar.gz
tar -xzvf openssl-1.0.2l.tar.gz
cd openssl-1.0.2l
sudo ./config
sudo make install
sudo ln -sf /usr/local/ssl/bin/openssl `which openssl`
openssl version -v
