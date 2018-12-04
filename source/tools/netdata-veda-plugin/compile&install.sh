go get github.com/BurntSushi/toml
go build veda.d.plugin.go queue.go tools.go

sudo service netdata stop
sudo rm /usr/libexec/netdata/plugins.d/veda.d.plugin
sudo cp veda.d.plugin /usr/libexec/netdata/plugins.d/
sudo cp veda-plugin-properties.ini /etc/netdata/
sudo service netdata start

