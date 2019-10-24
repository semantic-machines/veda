certbot -q renew --tls-sni-01-port=8888
cd /etc/letsencrypt/live/name.semantic-machines.com/
cat ./fullchain.pem ./privkey.pem | tee ./name.semantic-machines.com.pem
service haproxy reload