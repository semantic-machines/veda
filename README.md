# Veda system project [![Build Status](https://travis-ci.org/karpovr/veda.svg?branch=master)](https://travis-ci.org/karpovr/veda)

# I. Project dependencies
  See dub.json for dependencies list


veda install:

System requirements: RAM - 1Gb

**1: install dmd 2.066.1 and dub**
A) http://d-apt.sourceforge.net/
    sudo wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
    sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring && sudo apt-get update
    sudo apt-get install dmd-bin dub
    sudo apt-get install libraptor2-dev

B)
    http://downloads.dlang.org/releases/2014/dmd_2.066.1-0_amd64.deb

**2: install dependencies**
    sudo apt-get install libzmq3-dev
    sudo apt-get install libevent-pthreads-2.0-5
    sudo apt-get install libraptor2-dev
    sudo apt-get install libevent-dev libssl-dev

**3: get src**
    sudo apt-get install git
    git clone https://github.com/karpovr/veda.git

**4: Building & running veda:**

    cd /path/to/veda
    dub
