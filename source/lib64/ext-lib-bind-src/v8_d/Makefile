CC=g++
INCLUDES=`pkg-config --cflags msgpack`
LIBS=`pkg-config --libs msgpack`
CFLAGS=$(INCLUDES) $(LIBS) -std=c++11 -c -Wl,--start-group

all:
	g++ $(CFLAGS) cbor.cpp util8json.cpp cbor8json.cpp msgpack8individual.cpp v8d.cc    
	ar rvs libv8d.a *.o
	rm *.o
	mv libv8d.a ../../
