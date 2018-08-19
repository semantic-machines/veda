# Veda - enterprise application development platform 
[![Build Status](https://travis-ci.org/semantic-machines/veda.svg?branch=master)](https://travis-ci.org/semantic-machines/veda)

## I. Overview

**1. Driven by Semantic Web methodology**
  - Veda platform pursues Semantic Web methodology for data description, storage and exchange (https://en.wikipedia.org/wiki/Semantic_Web).

**2. Two-tier architecture**
  - Thin server (stateless).
  - Rich browser-side web-client (stateful).

**3. Server components**
  - Integrated LMDB database library. Key-value memory mapped storage (http://symas.com/mdb).
  - Integrated Xapian search engine library. Used for data indexing & querying (http://xapian.org).
  - Integrated V8 javascript engine library. Used for workflow execution engine and other server-side logic (https://code.google.com/p/v8).
  - Fasthttp web server.
  - Nanomsg - light-weight messaging protocol library.

**4. Client components**
  - Bootstrap HTML, CSS, and JS framework for developing responsive, mobile first web applications (http://getbootstrap.com).
  - Riot.js a React-like user interface micro-library. Used for implementing MVC pattern & client-side routing (http://riotjs.com).
  - jQuery general purpose client-side javascript library. Used for client-server interaction, event handling, DOM traversal & manipulation (https://jquery.com).
  - jsPlumb visual connectivity library for webapps. Used for workflow viewer/editor (http://jsplumbtoolkit.com).
  - Various visualization libraries & plugins.

## II. System requirements
  - OS - Ubuntu 14.04 LTS 64-Bit, 16.04 LTS 64-Bit
  - RAM - 1Gb

## III. How to install and run

**1. Install dependencies**
```sh
$ sudo control-install.sh
```
**2. Build & start veda**
```sh
$ build.sh
```
**3. Start/stop veda as daemon once built**
```sh
$ control-start.sh
$ control-stop.sh
```
