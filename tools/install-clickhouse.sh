#!/bin/bash

echo "deb http://repo.yandex.ru/clickhouse/deb/stable/ main/" | sudo tee /etc/apt/sources.list.d/clickhouse.list

sudo apt-get -y install dirmngr    # optional
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv E0C56BD4    # optional
sudo apt-get -y update
export DEBIAN_FRONTEND=noninteractive
sudo apt-get -yq install clickhouse-client clickhouse-server