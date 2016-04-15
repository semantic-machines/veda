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
sudo apt-get install libzmq-dev
mkdir tmp
cd tmp
git clone git://github.com/jedisct1/libsodium.git 
cd libsodium 
./autogen.sh 
./configure && make check 
sudo make install 
sudo ldconfig 
cd ..
wget http://download.zeromq.org/zeromq-4.1.4.tar.gz
tar -xvzf zeromq-4.1.4.tar.gz
cd zeromq-4.1.4
./autogen.sh
./configure
make
sudo make install
cd ..
cd ..

sudo ldconfig
