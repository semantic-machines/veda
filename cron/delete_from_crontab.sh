#!/bin/sh

#write out current crontab & filter previous "veda-trigger-..." entries
crontab -l | grep -vE "veda-trigger-" > current
#install new cron file
crontab current
rm current