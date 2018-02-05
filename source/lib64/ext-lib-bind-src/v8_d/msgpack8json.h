#ifndef MSGPACK8JSON_H
#define MSGPACK8JSON_H

#include "v8.h"
#include "msgpuck.h"

using namespace std;

v8::Handle<v8::Value> cbor2jsobject(v8::Isolate *isolate, string in_str);
void jsobject2cbor(v8::Local<v8::Value> value, v8::Isolate *isolate, std::vector<char> &ou);

#endif
