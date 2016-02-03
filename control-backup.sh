#!/bin/bash

# Encoding: UTF-8

TIMESTAMP=`date +%Y-%m-%d_%H_%M`
backup_path=./backup/optiflow-backup-$TIMESTAMP
mkdir $backup_path
export PATH="$PATH:/sbin/"

# Detele old backups
find ./backup -mmin +7200 -delete

# Stop optiflow
./control-stop.sh

zip $backup_path/data.zip ./data -r -x "./data/files/*"

# Start optiflow
./control-start.sh