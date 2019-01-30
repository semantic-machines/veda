#!/bin/sh

#write out current crontab & filter previous "veda-trigger-..." entries
crontab -l | grep -vE "veda-trigger\.sh [a-z]*" > current
#install new cron file
crontab current
rm current