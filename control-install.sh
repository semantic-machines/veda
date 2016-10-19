#!/bin/bash

# берет новые исходники из github, но не собирает

DMD_VER=2.070.2
DUB_VER=1.0.0

# Get right version of DMD
if ! dmd --version | grep $DMD_VER ; then    
    wget http://downloads.dlang.org/releases/2.x/$DMD_VER/dmd_$DMD_VER-0_amd64.deb
    sudo dpkg -i dmd_$DMD_VER-0_amd64.deb
    rm dmd_$DMD_VER-0_amd64.deb

    # Patch DMD
    sudo cp ./qa/patch_dmd_$DMD_VER/concurrency.d /usr/include/dmd/phobos/std/concurrency.d
    sudo cp ./qa/patch_dmd_$DMD_VER/concurrency.d ${HOME}/dmd2/src/phobos/std/concurrency.d
fi

# Get right version of DUB
if ! dub --version | grep $DUB_VER ; then
    wget http://code.dlang.org/files/dub-$DUB_VER-linux-x86_64.tar.gz
    tar -xvzf dub-$DUB_VER-linux-x86_64.tar.gz    
    sudo cp ./dub /usr/bin/dub
    rm dub-$DUB_VER-linux-x86_64.tar.gz
    rm dub
fi

# Get other dependencies
LIB_NAME[1]="libevent-pthreads-2.0-5"
LIB_NAME[2]="libraptor2-dev"
LIB_NAME[3]="libevent-dev"
LIB_NAME[4]="libssl-dev"
LIB_NAME[5]="libmysqlclient-dev"
LIB_NAME[6]="cmake"
LIB_NAME[7]="libtool"
LIB_NAME[8]="pkg-config"
LIB_NAME[9]="build-essential"
LIB_NAME[10]="autoconf"
LIB_NAME[11]="automake"

LIB_OK="Status: install ok installed"
F_UL=0

for i in "${LIB_NAME[@]}"; do

    L1=`dpkg -s $i | grep 'install ok'`

    if  [ "$L1" != "$LIB_OK" ]; then

	if [ $F_UL == 0 ]; then
	    sudo apt-get update
	    F_UL=1
	fi 

        sudo apt-get install -y $i
    fi

done

if ! ldconfig -p | grep libwebsockets; then

    # make libwebsockets dependency
    mkdir tmp
    wget https://github.com/warmcat/libwebsockets/archive/v2.0.1.tar.gz -P tmp
    cd tmp
    tar -xvzf v2.0.1.tar.gz
    cd libwebsockets-2.0.1
    mkdir build
    cd build
    cmake ..
    make
    sudo make install
    sudo ldconfig
    cd ..
    cd ..
    cd ..

fi

if ! ldconfig -p | grep libnanomsg; then

    # make nanomsg dependency
    mkdir tmp
    wget https://github.com/nanomsg/nanomsg/archive/1.0.0.tar.gz -P tmp
    cd tmp
    tar -xvzf 1.0.0.tar.gz
    cd nanomsg-1.0.0
    mkdir build
    cd build
    cmake ..
    make
    sudo make install
    sudo ldconfig
    cd ..
    cd ..
    cd ..

fi
