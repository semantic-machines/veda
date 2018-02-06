#ifndef UTIL8JSON_H_INCLUDED
#define UTIL8JSON_H_INCLUDED

#include <string>
#include <sstream>
#include "v8.h"

using namespace std;
using namespace v8;

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

void double_to_mantissa_exponent(double inp, int64_t *mantissa, int64_t *exponent);
string exponent_and_mantissa_to_string(long decimal_mantissa_data, long decimal_exponent_data);
bool jsobject_log(Local<Value> value);
template < typename T > std::string to_string( const T& n );   
int64_t to_int(char const *s);

#endif
