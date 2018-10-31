#!/bin/bash

# You must have Ruby development kit on your computer.
# To install jsduck run: gem install jsduck
# You also need `make` and `ruby-dev` packages
jsduck --title="Veda Docs" \
       --output=./public/docs/js \
       --exclude=./public/js/browser/lib \
       --exclude=./public/js/common/lib \
       --exclude=./public/js/!not_used \
       --exclude=./public/js/test \
       ./public/js