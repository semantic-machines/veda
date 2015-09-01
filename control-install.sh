# Variant I
#sudo wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
#sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring && sudo apt-get update
#sudo apt-get install dmd-bin dub
#sudo apt-get install libraptor2-dev

# Variant II
if ! dmd --version | grep 2.067.1 ; then
    wget http://downloads.dlang.org/releases/2.x/2.068.0/dmd_2.068.0-0_amd64.deb
    sudo dpkg -i dmd_2.068.0-0_amd64.deb
    sudo cp ./qa/patch_dmd_2_68_0/concurrency.d /usr/include/dmd/phobos/std/concurrency.d
fi

if ! dub --version | grep 0.9.23 ; then
    wget http://code.dlang.org/files/dub-0.9.23-linux-x86_64.tar.gz
    tar -xvzf dub-0.9.23-linux-x86_64.tar.gz
    sudo cp ./dub /usr/bin/dub
fi

sudo apt-get install libzmq3-dev
sudo apt-get install libevent-pthreads-2.0-5
sudo apt-get install libraptor2-dev
sudo apt-get install libevent-dev libssl-dev


