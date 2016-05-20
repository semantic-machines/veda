# берет новые исходники из github, но не собирает

DMD_VER=2.070.0
DUB_VER=0.9.24

sudo apt-get update

# Get right version of DMD
if ! dmd --version | grep $DMD_VER ; then    
    wget http://downloads.dlang.org/releases/2.x/$DMD_VER/dmd_$DMD_VER-0_amd64.deb
    sudo dpkg -i dmd_$DMD_VER-0_amd64.deb
    rm dmd_$DMD_VER-0_amd64.deb
fi
# Patch DMD
sudo cp ./qa/patch_dmd_$DMD_VER/concurrency.d /usr/include/dmd/phobos/std/concurrency.d
sudo cp ./qa/patch_dmd_$DMD_VER/concurrency.d ${HOME}/dmd2/src/phobos/std/concurrency.d

# Get right version of DUB
if ! dub --version | grep $DUB_VER ; then
    wget http://code.dlang.org/files/dub-$DUB_VER-linux-x86_64.tar.gz
    tar -xvzf dub-$DUB_VER-linux-x86_64.tar.gz    
    sudo cp ./dub /usr/bin/dub
    rm dub-$DUB_VER-linux-x86_64.tar.gz
    rm dub
fi

# Get other dependencies
sudo apt-get install -y libevent-pthreads-2.0-5
sudo apt-get install -y libraptor2-dev
sudo apt-get install -y libevent-dev libssl-dev
sudo apt-get install -y libmysqlclient-dev
sudo apt-get install libtool pkg-config build-essential autoconf automake
sudo apt-get install cmake

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