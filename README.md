# Veda system project [![Build Status](https://travis-ci.org/karpovr/veda.svg?branch=master)](https://travis-ci.org/karpovr/veda)

## I. System requirements:
  - OS - Ubuntu 12.04 LTS, 14.04 LTS
  - RAM - 1Gb

## II. How to install:


**1: install dmd 2.068.0 and dub**
  - A) http://d-apt.sourceforge.net/
```sh
    sudo wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
    sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring && sudo apt-get update
    sudo apt-get install dmd-bin dub
    sudo apt-get install libraptor2-dev
```
  - B) http://downloads.dlang.org/releases/2014/dmd_2.068.0-0_amd64.deb

**2: install dependencies**
```sh
    sudo apt-get install libzmq3-dev
    sudo apt-get install libevent-pthreads-2.0-5
    sudo apt-get install libraptor2-dev
    sudo apt-get install libevent-dev libssl-dev
```

**3: get src**
```sh
    sudo apt-get install git
    git clone https://github.com/karpovr/veda.git
```

**4: Building & running veda:**
```sh
    cd /path/to/veda
    dub
```
