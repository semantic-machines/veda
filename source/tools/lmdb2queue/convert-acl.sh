#!/bin/bash

# convert acl db
mkdir input
mkdir input/lmdb
mkdir out
mkdir out/queue

./lmdb2queue string acl
