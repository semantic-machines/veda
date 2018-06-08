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

dbuser=ba
dbname=veda_db
dbpassword=,f,ehtxyfz691
echo "Enter root password for MYSQL connect"
mysql -u root -p -e "create user '$dbuser'@'localhost';
create database $dbname;
grant all on $dbname.* to '$dbuser'@'localhost';
set password for '$dbuser'@'localhost' = password('$dbpassword');"
if [ $? != 0 ]; then
  echo "Some errors ocured. Check STDOUT to see."
else
  echo "DB $dbname created. User $dbuser created."
fi

sudo sed -i '27a\skip-external-locking\nlower_case_table_names=1\ncollation-server = utf8_unicode_ci\ninit-connect="SET NAMES utf8"\ncharacter-set-server = utf8\nsql_mode=NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION' /etc/mysql/mysql.conf.d/mysqld.cnf
if [ $? != 0 ]; then
  echo "Some errors ocured. Check STDOUT to see."
else
  echo "Strings add into mysqld.cnf."
fi

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

