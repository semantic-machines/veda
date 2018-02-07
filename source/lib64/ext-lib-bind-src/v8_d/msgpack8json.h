#ifndef MSGPACK8JSON_H
#define MSGPACK8JSON_H

#include <vector>
#include "v8.h"

using namespace std;

v8::Handle<v8::Value> msgpack2jsobject(v8::Isolate *isolate, string in_str);
void jsobject2msgpack(v8::Local<v8::Value> value, v8::Isolate *isolate, std::vector<char> &ou);

#endif
