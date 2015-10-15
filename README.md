# Veda - enterprise application development platform [![Build Status](https://travis-ci.org/karpovr/veda.svg?branch=master)](https://travis-ci.org/karpovr/veda)

## I. Overview

**1) Driven by Semantic Web methodology
  - Veda platform pursues Semantic Web methodology for data description, storage and exchange (https://en.wikipedia.org/wiki/Semantic_Web).

**2) Two-tier architecture:
  - Thin server (stateless).
  - Rich browser-side web-client (stateful).

**3) Server components:
  - Integrated LMDB database library. Key-value memory mapped storage (http://symas.com/mdb).
  - Integrated Xapian search engine library. Used for data indexing & querying (http://xapian.org).
  - Integrated V8 javascript engine library. Used for workflow execution engine and other server-side logic (https://code.google.com/p/v8).
  - Vibe - high-level declarative REST and web application framework (http://vibed.org).

**4) Client components:
  - Bootstrap HTML, CSS, and JS framework for developing responsive, mobile first web applications (http://getbootstrap.com).
  - Riot.js a React-like user interface micro-library. Used for implementing MVC pattern & client-side routing (http://riotjs.com).
  - jQuery general purpose client-side javascript library. Used for client-server interaction, event handling, DOM traversal & manipulation (https://jquery.com).
  - jsPlumb visual connectivity library for webapps. Used for workflow viewer/editor (http://jsplumbtoolkit.com).
  - Various visualisation libraries & plugins.

## II. System requirements:
  - OS - Ubuntu 12.04 LTS, 14.04 LTS
  - RAM - 1Gb

## III. How to install:

**1: install dmd 2.068.2 and dub**
  - A) http://d-apt.sourceforge.net/
```sh
    sudo wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
    sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring && sudo apt-get update
    sudo apt-get install dmd-bin dub
    sudo apt-get install libraptor2-dev
```
  - B) http://downloads.dlang.org/releases/2014/dmd_2.068.2-0_amd64.deb

**2: install dependencies**
```sh
    sudo apt-get install libzmq3-dev
    sudo apt-get install libevent-pthreads-2.0-5
    sudo apt-get install libraptor2-dev
    sudo apt-get install libevent-dev libssl-dev
    sudo apt-get install libmysqlclient-dev
```

**3: get src**
```sh
    sudo apt-get install git
    git clone https://github.com/karpovr/veda.git
```

**4: Building & running veda:**
```sh
    cd /path/to/veda
    dub
```
