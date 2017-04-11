service netdata stop

go build veda.d.plugin.go
sudo cp veda.d.plugin /usr/libexec/netdata/plugins.d/

sudo service netdata start

