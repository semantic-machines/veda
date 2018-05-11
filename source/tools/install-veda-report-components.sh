############################################

adduser sedadm
adduser sedadm sudo
sudo locale-gen "en_US.UTF-8"
sudo dpkg-reconfigure locales 

############################################

apt-get install software-properties-common

# install java 8
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer
sudo apt-get install oracle-java8-set-default
java -version

# install mysql
sudo apt-get install mysql-server
sudo apt-get install mysql-client

#configure mysql for veda

mysql -u root -p
CREATE USER 'ba'@'localhost' IDENTIFIED BY ',f,ehtxyfz69';
GRANT ALL PRIVILEGES ON * . * TO 'ba'@'localhost';
CREATE DATABASE veda_db;

add to file mysqld.cnf:

[mysqld]
skip-external-locking
lower_case_table_names=1
collation-server = utf8_unicode_ci
init-connect='SET NAMES utf8'
character-set-server = utf8
sql_mode=NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION

sudo /etc/init.d/mysql restart

# install jasperreport server
wget https://netcologne.dl.sourceforge.net/project/jasperserver/JasperServer/JasperReports%20Server%20Community%20Edition%206.1.0/jasperreports-server-cp-6.1.0-linux-x64-installer.run

обновить jasperreports-server-cp-6.1.0/java/jre/lib/fonts

crontab -e
@reboot cd /home/sedadm/veda && ./control-stop.sh && ./control-start.sh
@reboot cd /home/sedadm/jasperreports-server-cp-6.1.0 && ./ctlscript.sh
00 * * * * /home/sedadm/veda/cron/veda-trigger-hourly.sh
10 9 * * * /home/sedadm/veda/cron/veda-trigger-daily.sh
20 0 * * 7 /home/sedadm/veda/cron/veda-trigger-weekly.sh
30 0 1 * * /home/sedadm/veda/cron/veda-trigger-monthly.sh
40 0 1 1 * /home/sedadm/veda/cron/veda-trigger-yearly.sh

00 2 * * * cd /home/sedadm/veda && ./control-backup.sh

