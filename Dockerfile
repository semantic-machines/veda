FROM ubuntu:14.04.3
MAINTAINER Maksimenko Vladimir <xupypr@xupypr.com>
LABEL Description="This is the default image for VEDA" Vendor="semantic-machines.com" Version="1.0"

ENV DEBIAN_FRONTEND noninteractive 
RUN locale-gen ru_RU.UTF-8 && dpkg-reconfigure locales 

RUN apt-get update

RUN apt-get -y install wget
RUN apt-get -y install git

RUN git clone https://github.com/semantic-machines/veda.git

RUN cd ./veda && ./control-install.sh

RUN apt-get -y -f install

RUN cd ./veda && ./control-install.sh

RUN cd ./veda && ./m.sh

RUN cd ./veda && ./control-start.sh

RUN /bin/bash