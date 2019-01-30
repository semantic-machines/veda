#!/bin/sh

#write out current crontab & filter previous "veda-trigger-..." entries
crontab -l | grep -vE "veda-trigger\.sh [a-z]*" > current
#echo new entries into cron file
echo "00 15 * * * `pwd`/veda-trigger.sh hourly" >> current
echo "00 15 * * * `pwd`/veda-trigger.sh daily" >> current
echo "00 15 * * * `pwd`/veda-trigger.sh weekly" >> current
echo "00 15 * * * `pwd`/veda-trigger.sh monthly" >> current
echo "00 15 * * * `pwd`/veda-trigger.sh yearly" >> current
#install new cron file
crontab current
rm current