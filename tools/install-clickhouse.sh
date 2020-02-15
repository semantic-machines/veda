#!/bin/bash

sudo apt-get -y install dirmngr    # optional
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv E0C56BD4    # optional
sudo apt-get -y update
sudo apt-get -y install clickhouse-client clickhouse-server