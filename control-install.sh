#!/bin/bash
# скрипт устанавливает среду для последующей компиляции, берет исходники зависимостей из github, но не собирает

DMD_VER=2.080.0
DUB_VER=1.5.0
GO_VER=go1.11
MSGPUCK_VER=2.0
TARANTOOL_VER=1.10.2
NANOMSG_VER=1.1.5

INSTALL_PATH=$PWD

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
LIB_NAME[12]="curl"

LIB_OK="Status: install ok installed"
F_UL=0

### LIBS FROM APT ###

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

sudo apt-get install build-essential

### RUST LANG ###

if ! rustc -V; then
    echo "--- INSTALL RUST ---"
    curl https://sh.rustup.rs -sSf | sh -s -- -y
    source $HOME/.cargo/env
else
    echo "--- UPDATE RUST ---"
    rustup update stable
fi

whereis rustc
rustc -V
cargo -V

### D LANG ###

# Get right version of DMD
if ! dmd --version | grep $DMD_VER ; then
    echo "--- INSTALL DMD ---"
    wget -w 10 http://downloads.dlang.org/releases/2.x/$DMD_VER/dmd_$DMD_VER-0_amd64.deb
    sudo dpkg -i dmd_$DMD_VER-0_amd64.deb
    rm dmd_$DMD_VER-0_amd64.deb
    rm -r ~/.dub
else
    echo "--- DMD INSTALLED ---"
fi

# Get right version of DUB
if ! dub --version | grep $DUB_VER ; then
    echo "--- INSTALL DUB ---"
    wget https://github.com/dlang/dub/releases/download/v$DUB_VER/dub-$DUB_VER-linux-x86_64.tar.gz
    tar -xvzf dub-$DUB_VER-linux-x86_64.tar.gz
    sudo cp ./dub /usr/bin/dub
    rm dub-$DUB_VER-linux-x86_64.tar.gz
    rm dub
else
    echo "--- DUB INSTALLED ---"
fi

### GO LANG ###
if ! go version | grep $GO_VER ; then
    echo "--- INSTALL GOLANG ---"
    mkdir tmp
    cd tmp
    wget https://storage.googleapis.com/golang/$GO_VER.linux-amd64.tar.gz
    tar -xf $GO_VER.linux-amd64.tar.gz

    if env | grep -q ^GOROOT=
    then
        sudo rm -rf $GOROOT
    else
        export GOROOT=/usr/local/go
        export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"
        echo 'export GOROOT=/usr/local/go'  >> $HOME/.profile
        echo 'export PATH=$PATH:$GOROOT/bin:$GOPATH/bin'  >> $HOME/.profile
    fi

    export GOPATH=$HOME/go
    echo 'export GOPATH=$HOME/go'  >> $HOME/.bashrc
    source ~/.bashrc

    sudo rm -rf /usr/local/go
    sudo rm -rf /usr/bin/go
    sudo rm -rf /usr/bin/gofmt
    sudo mv go $GOROOT

    go version
    cd ..
else
    echo "--- GOLANG INSTALLED ---"
fi

#lmdb-go
#go get -v github.com/muller95/lmdb-go/lmdb
go get github.com/itiu/lmdb-go/lmdb

#fasthttp
go get -v github.com/itiu/fasthttp

#go-nanomsg
go get -v github.com/op/go-nanomsg

go get github.com/tarantool/go-tarantool
go get github.com/gorilla/websocket
go get github.com/divan/expvarmon
go get -v gopkg.in/vmihailenco/msgpack.v2
cp -a ./source/golang-third-party/cbor $GOPATH/src
ls $HOME/go

### TARANTOOL SERVER ###

if ! tarantool -V | grep $TARANTOOL_VER; then
echo "--- INSTALL TARANTOOL ---"
curl http://download.tarantool.org/tarantool/1.10/gpgkey | sudo apt-key add -
release=`lsb_release -c -s`

# install https download transport for APT
sudo apt-get -y install apt-transport-https

# append two lines to a list of source repositories
sudo rm -f /etc/apt/sources.list.d/*tarantool*.list
sudo tee /etc/apt/sources.list.d/tarantool_1_0.list <<- EOF
deb http://download.tarantool.org/tarantool/1.10/ubuntu/ $release main
deb-src http://download.tarantool.org/tarantool/1.10/ubuntu/ $release main
EOF

# install
sudo apt-get update
sudo apt-get remove tarantool
sudo apt-get remove tarantool-dev
sudo apt-get -y install tarantool
sudo apt-get -y install tarantool-dev

tarantool -V

else
    echo "--- TARANTOOL INSTALLED ---"
fi

### LIB NANOMSG ###

if ! ldconfig -p | grep libnanomsg; then
    echo "--- INSTALL NANOMSG ---"
    # make nanomsg dependency
    mkdir tmp
    wget https://github.com/nanomsg/nanomsg/archive/$NANOMSG_VER.tar.gz -P tmp
    cd tmp
    tar -xvzf $NANOMSG_VER.tar.gz
    cd nanomsg-$NANOMSG_VER
    mkdir build
    cd build
    cmake ..
    make
    sudo make install

    echo '/usr/local/lib/x86_64-linux-gnu' > x86_64-linux-gnu-local.conf
    sudo cp x86_64-linux-gnu-local.conf /etc/ld.so.conf.d/x86_64-linux-gnu-local.conf
    sudo ldconfig

    cd ..
    cd ..
    cd ..
else
    echo "--- NANOMSG INSTALLED ---"
fi

### LIB RAPTOR ###

sudo apt-get remove -y libraptor2-0
ldconfig -p | grep libraptor2
if ! ldconfig -p | grep libraptor2; then
    echo "--- INSTALL LIB RAPTOR ---"
    sudo apt-get install -y gtk-doc-tools
    sudo apt-get install -y libxml2-dev
    sudo apt-get install -y flex
    sudo apt-get install -y bison

    mkdir tmp
    cd tmp

    wget https://github.com/dajobe/raptor/archive/raptor2_2_0_15.tar.gz -P .
    tar -xvzf raptor2_2_0_15.tar.gz

    cd raptor-raptor2_2_0_15
    autoreconf -i
    ./autogen.sh
    ./make
    sudo make install
    sudo ldconfig
    cd ..
    cd ..
    cd ..

else
    echo "--- LIB RAPTOR INSTALLED ---"
fi

if ! ldconfig -p | grep libtarantool; then
    echo "--- INSTALL LIBTARANTOOL ---"
    TTC=213ed9f4ef8cc343ae46744d30ff2a063a8272e5

    mkdir tmp
    cd tmp

    wget https://github.com/tarantool/tarantool-c/archive/$TTC.tar.gz -P .
    tar -xvzf $TTC.tar.gz

    wget https://github.com/tarantool/msgpuck/archive/$MSGPUCK_VER.tar.gz -P third_party/msgpuck -P .
    tar -xvzf $MSGPUCK_VER.tar.gz

    cp msgpuck-$MSGPUCK_VER/* tarantool-c-$TTC/third_party/msgpuck
    cd tarantool-c-$TTC

    mkdir build
    cd build
    cmake ..
    make
    sudo make install
    sudo ldconfig

    cd ..
    cd ..

else
    echo "--- LIBTARANTOOL INSTALLED ---"
fi

    cd $INSTALL_PATH
    cd source/authorization
    cargo build --release
    cd ..
    cd ..
    sudo cp ./source/lib64/libauthorization.so /usr/local/lib
    sudo ldconfig
