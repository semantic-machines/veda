#!/bin/sh
echo "creating lib..."
rm *.o
rm libv8d.a
rm d_test_v8d
g++ -c -Wl,--start-group cbor.cpp cbor2individual.cpp v8d.cc
#g++ -Wall -shared -fPIC -o libxapiand.so -L-lxapian *.cpp
ar rvs libv8d.a *.o
#echo "creating dmd-test..."
#dmd -O -g d_test_v8d.d v8d_header.d libv8d.a lib/libv8_base.x64.a lib/libicui18n.a lib/libicuuc.a lib/libicudata.a lib/libv8_nosnapshot.x64.a lib/libstdc++.a
rm *.o
mv libv8d.a ../../lib64