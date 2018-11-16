#!/bin/bash

# Encoding: UTF-8

TIMESTAMP=`date +%Y-%m-%d_%H_%M`
backup_path=./backup/optiflow-backup-$TIMESTAMP
mkdir $backup_path
export PATH="$PATH:/sbin/"

# Stop optiflow
./control-stop.sh

# Create backup of databases
#zip $backup_path/data.zip ./data -1 -r -x "./data/files/*"
zip $backup_path/data-ft-base.gz ./data/xapian-info ./data/xapian-search-base -r -1
zip $backup_path/data-ft-deleted.gz ./data/xapian-search-deleted -r -1
zip $backup_path/data-ft-system.gz ./data/xapian-search-system -r -1
zip $backup_path/data-acl.zip ./data/acl-indexes -r -0
zip $backup_path/data-indv.zip ./data/lmdb-individuals ./data/uris -r -0
zip $backup_path/data-queue.zip ./data/lmdb-tickets ./data/module-info ./data/queue -r -0
zip $backup_path/data-trails.zip ./data/trails -r -1

# Craete backup of logs
zip $backup_path/log.zip ./logs/*.log -1 -r

# Backup config.ttl
cp ./ontology/config.ttl $backup_path/config.ttl

# Detele old backups and logs
find ./data/lmdb-individuals.* -exec mv {} ./backup/binlogs \;
find ./data/lmdb-tickets.* -delete
find ./logs/*.log -delete
rm ./veda.log
rm -r ./data/trails
find ./backup/optiflow-backup* -mmin +7200 -delete

# Start optiflow
./control-start.sh
