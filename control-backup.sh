#!/bin/bash

# Encoding: UTF-8

TIMESTAMP=`date +%Y-%m-%d_%H_%M`
backup_path=./backup/optiflow-backup-$TIMESTAMP
mkdir $backup_path
export PATH="$PATH:/sbin/"

# Stop optiflow
./control-stop.sh

# Create backup of databases
zip $backup_path/data.zip ./data -r -x "./data/files/*"

# Craete backup of logs
zip $backup_path/log.zip ./*.log -r

# Backup config.ttl
cp ./ontology/config.ttl $backup_path/config.ttl

# Detele old backups and logs
find ./data/lmdb-individuals.* -exec mv {} ./backup-binlogs \;
find ./data/lmdb-tickets.* -delete
find ./*.log -delete
rm ./veda.log
find ./backup/optiflow-backup* -mmin +7200 -delete

# Start optiflow
./control-start.sh