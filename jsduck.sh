# You must have Ruby development kit on your computer.
# To install jsduck run: gem install jsduck
jsduck --title="Veda Docs" \
       --output=./docs \
       --exclude=./public/js/browser/lib \
       --exclude=./public/js/common/lib \
       --exclude=./public/js/!not_used \
       --exclude=./public/js/test \
       ./public/js