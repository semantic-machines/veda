sudo apt-get update

# Get right version of DMD
if ! dmd --version | grep 2.068.2 ; then    
    wget http://downloads.dlang.org/releases/2.x/2.068.2/dmd_2.068.2-0_amd64.deb
    sudo dpkg -i dmd_2.068.2-0_amd64.deb
    rm dmd_2.068.2-0_amd64.deb
fi
# Patch DMD
sudo cp ./qa/patch_dmd_2_68_0/concurrency.d /usr/include/dmd/phobos/std/concurrency.d
sudo cp ./qa/patch_dmd_2_68_0/concurrency.d ${HOME}/dmd2/src/phobos/std/concurrency.d

# Get right version of DUB
if ! dub --version | grep 0.9.23 ; then
    wget http://code.dlang.org/files/dub-0.9.23-linux-x86_64.tar.gz
    tar -xvzf dub-0.9.23-linux-x86_64.tar.gz    
    sudo cp ./dub /usr/bin/dub
    rm dub-0.9.23-linux-x86_64.tar.gz
    rm dub
fi

# Get other dependencies
sudo apt-get install -y libzmq3-dev
sudo apt-get install -y libevent-pthreads-2.0-5
sudo apt-get install -y libraptor2-dev
sudo apt-get install -y libevent-dev libssl-dev
sudo apt-get install -y libmysqlclient-dev

sudo ldconfig