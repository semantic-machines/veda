---
layout: default
title: Быстрый старт
permalink: /05gettingstarted/
---

### Содержание

1. [Установка на Ubuntu 14.04 LTS 64-Bit](#ubuntu)
1. [Установка на Docker](#docker)
1. [Создание структуры : "Служебная записка"](#memo-create)
1. [Создание кастомного шаблона : "Служебная записка"](#memo-template)
1. [Создание маршрута документооборота : "Служебная записка"](#memo-workflow)

<h1 id="ubuntu" class="page-heading">Установка на Ubuntu 14.04 LTS 64-Bit</h1>

1) Скопировать исходные коды из репозитария :
{% highlight bash %}
git clone https://github.com/semantic-machines/veda.git
cd ./veda
{% endhighlight %}
2) Установить дополнительные пакеты :
{% highlight bash %}
sudo ./control-install.sh
{% endhighlight %}
3) Запустить сборку проекта :
{% highlight bash %}
dub
{% endhighlight %}
4) Запустить проект :
{% highlight bash %}
./control-start.sh
{% endhighlight %}
5) VEDA будет запущена автоматически после сборки. Также запустить VEDA можно без сборки :
{% highlight bash %}
./veda
{% endhighlight %}
6) Откройте веб-интерфейс VEDA при помощи вашего любимого браузера:
{% highlight bash %}
http://localhost:8080/
{% endhighlight %}

<h1 id="docker" class="page-heading">Установка на Docker</h1>
1) Создайте папку, в которой будут хранится данные veda:
{% highlight bash %}
mkdir veda
cd veda
{% endhighlight %}
2) Запустите Docker контейнер
{% highlight bash %}
docker run --rm -p 8080:8080 -v $PWD:/veda/data -ti semanticmachines/veda /bin/bash -c "cd /veda && ./veda"
{% endhighlight %}
3) Откройте веб-интерфейс VEDA при помощи вашего любимого браузера:
{% highlight bash %}
http://localhost:8080/
{% endhighlight %}

