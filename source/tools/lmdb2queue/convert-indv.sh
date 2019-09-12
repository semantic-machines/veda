#!/bin/bash

# convert individuals db
mkdir input
mkdir input/lmdb
mkdir out
mkdir out/queue

./lmdb2queue individual individuals simple_queue
