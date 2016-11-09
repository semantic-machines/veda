#!/bin/bash

# берет новые исходники из github, но не собирает

DMD_VER=2.072.0
DUB_VER=1.0.0

# Get right version of DMD
if ! dmd --version | grep $DMD_VER ; then    
    wget http://downloads.dlang.org/releases/2.x/$DMD_VER/dmd_$DMD_VER-0_amd64.deb
    sudo dpkg -i dmd_$DMD_VER-0_amd64.deb
    rm dmd_$DMD_VER-0_amd64.deb
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
    wget https://github.com/warmcat/libwebsockets/archive/v2.0.3.tar.gz -P tmp
    cd tmp
    tar -xvzf v2.0.3.tar.gz
    cd libwebsockets-2.0.3
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

if ! ldconfig -p | grep libtraildb; then

    sudo apt-get install libarchive-dev pkg-config
    sudo apt-get remove libjudydebian1
    sudo apt-get remove libjudy-dev

    mkdir tmp
    cd tmp

    wget https://mirrors.kernel.org/ubuntu/pool/universe/j/judy/libjudy-dev_1.0.5-5_amd64.deb \
     https://mirrors.kernel.org/ubuntu/pool/universe/j/judy/libjudydebian1_1.0.5-5_amd64.deb
    sudo dpkg -i libjudy-dev_1.0.5-5_amd64.deb libjudydebian1_1.0.5-5_amd64.deb


    wget https://github.com/traildb/traildb/archive/0.5.tar.gz -P tmp
    cd tmp
    tar -xvzf 0.5.tar.gz

    cd traildb-0.5
    ./waf configure
    ./waf build
    sudo ./waf install
    sudo ldconfig
    cd ..
    cd ..
    cd ..
fi

if ! ldconfig -p | grep libraptor; then

    mkdir tmp
    cd tmp

    wget http://download.librdf.org/source/raptor2-2.0.15.tar.gz -P tmp
    cd tmp
    tar -xvzf raptor2-2.0.15.tar.gz

    cd raptor2-2.0.15
    ./configure
    ./make
    sudo make install
    sudo ldconfig
    cd ..
    cd ..
    cd ..

fi
