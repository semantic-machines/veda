#ifndef UTIL8JSON_H_INCLUDED
#define UTIL8JSON_H_INCLUDED

#include "v8.h"

using namespace std;
using namespace v8;

void double_to_mantissa_exponent(double inp, int64_t *mantissa, int64_t *exponent);

string exponent_and_mantissa_to_string(long decimal_mantissa_data, long decimal_exponent_data);

bool jsobject_log(Local<Value> value);

#endif
