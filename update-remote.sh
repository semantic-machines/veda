# 1. Run `control-stop.sh` on remote server (if server has no internet connection. Otherwise, don't use this sript, use `control-update.sh` instead. )
# 2. Run `update-remote.sh` and specify remote folder with veda
# 3. Run `cotrol-start.sh` on remote server

if [ -n "$1" ];
then
  if [ ! -f $1/veda ];
  then
     echo "You must specify folder with already installed VEDA"
  else
     #Ontology
     cp -LR ./ontology $1
     #Public
     cp -R ./public  $1
     #Veda
     cp ./veda $1
  fi
else
    echo "You must specify remote folder. Example: update-remote.sh /home/me/remote/server/path-to-veda/"
fi