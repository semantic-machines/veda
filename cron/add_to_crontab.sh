#!/bin/sh

#write out current crontab & filter previous "veda-trigger-..." entries
crontab -l | grep -vE "veda-trigger\.sh [a-z]*" > current
#echo new entries into cron file
echo "00 * * * * `pwd`/veda-trigger.sh hourly" >> current
echo "10 0 * * * `pwd`/veda-trigger.sh daily" >> current
echo "20 0 * * 7 `pwd`/veda-trigger.sh weekly" >> current
echo "30 0 1 * * `pwd`/veda-trigger.sh monthly" >> current
echo "40 0 1 1 * `pwd`/veda-trigger.sh yearly" >> current
#install new cron file
crontab current
rm current
