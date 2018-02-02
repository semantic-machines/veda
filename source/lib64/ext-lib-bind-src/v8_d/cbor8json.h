#ifndef CBOR8JSON_H
#define CBOR8JSON_H

#include "v8.h"
#include "cbor.h"

using namespace std;

struct Element
{
    unsigned int pos;
    string       str;

    Element () : pos(0), str("")
    {
    };
};

typedef enum
{
    LANG_NONE = 0,
    LANG_RU   = 1,
    LANG_EN   = 2
} tLANG;

typedef enum ResourceType
{
    _Uri      = 1,
    _String   = 2,
    _Integer  = 4,
    _Datetime = 8,
    _Decimal  = 32,
    _Boolean  = 64
} tResourceType;

v8::Handle<v8::Value> cbor2jsobject(v8::Isolate *isolate, string in_str);
void jsobject2cbor(v8::Local<v8::Value> value, v8::Isolate *isolate, std::vector<char> &ou);

#endif
