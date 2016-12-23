#!/bin/sh

#write out current crontab & filter previous "veda-trigger-..." entries
crontab -l | grep -vE "veda-trigger-[a-z]*\.sh" > current
#echo new entries into cron file
echo "00 * * * * `pwd`/veda-trigger-hourly.sh" >> current
echo "10 0 * * * `pwd`/veda-trigger-daily.sh" >> current
echo "20 0 * * 7 `pwd`/veda-trigger-weekly.sh" >> current
echo "30 0 1 * * `pwd`/veda-trigger-monthly.sh" >> current
echo "40 0 1 1 * `pwd`/veda-trigger-yearly.sh" >> current
#install new cron file
crontab current
rm current