#define _GLIBCXX_USE_CXX11_ABI    0

#include "v8.h"
#include <assert.h>
#include <iostream>
#include <string>
#include <string.h>
#include <math.h>
#include <sstream>
#include <limits>
#include <iomanip>
#include <cstdlib>
#include <cassert>
#include <cstddef>
#include "cbor.h"
#include <algorithm>

using namespace std;
using namespace v8;

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

typedef enum ResourceOrigin
{
    _local    = 1,
    _external = 2
} tResourceOrigin;

struct Element
{
    unsigned int pos;
    string       str;

    Element () : pos(0), str("")
    {
    };
};

//////////////////////////////////////////////////////////////////

namespace
{
void FatalErrorCallback_r(const char *location, const char *message)
{
    std::cerr << "Fatal error in V8: " << location << " " << message;
}
}

string nullz = "00000000000000000000000000000000";

string exponent_and_mantissa_to_string(long decimal_mantissa_data, long decimal_exponent_data)
{
    string str_res;
    string sign = "";
    string str_mantissa;

    if (decimal_mantissa_data < 0)
    {
        sign         = "-";
        str_mantissa = to_string(decimal_mantissa_data * -1);
    }
    else
        str_mantissa = to_string(decimal_mantissa_data);

    long lh = decimal_exponent_data * -1;

    lh = str_mantissa.length() - lh;
    string slh;

    if (lh >= 0)
    {
        if (lh <= str_mantissa.length())
            slh = str_mantissa.substr(0, lh);
    }
    else
        slh = "";

    string slr;

    if (lh >= 0)
    {
        slr = str_mantissa.substr(lh, str_mantissa.length());
    }
    else
    {
        slr = nullz.substr(0, (-lh)) + str_mantissa;
    }

    string ss;

    if (slr.length() == 0)
    {
        ss = sign + slh;
    }
    else
    {
        if (slh.length() == 0)
            slh = "0";
        ss = sign + slh + "." + slr;
    }

    return ss;
}

/// Stringify V8 value to JSON
/// return empty string for empty value
std::string json_str(v8::Isolate *isolate, v8::Handle<v8::Value> value)
{
    if (value.IsEmpty())
    {
        return std::string();
    }

    v8::HandleScope             scope(isolate);

    v8::Local<v8::Object>       json = isolate->GetCurrentContext()->
                                       Global()->Get(v8::String::NewFromUtf8(isolate, "JSON"))->ToObject();
    v8::Local<v8::Function>     stringify = json->Get(v8::String::NewFromUtf8(isolate, "stringify")).As<v8::Function>();

    v8::Local<v8::Value>        result = stringify->Call(json, 1, &value);
    v8::String::Utf8Value const str(result);

    return std::string(*str, str.length());
}

Handle<Value> cbor2jsobject(Isolate *isolate, string in_str)
{
    Handle<Object> js_map = Object::New(isolate);

    Handle<Value>  f_data = String::NewFromUtf8(isolate, "data");
    Handle<Value>  f_lang = String::NewFromUtf8(isolate, "lang");
    Handle<Value>  f_type = String::NewFromUtf8(isolate, "type");
    const char     *src   = in_str.c_str();

    ElementHeader  header;
    Element        element;

    int            size = in_str.size();

    element.pos = read_type_value(src, 0, size, &header);
    int n_resources = header.v_long;
    // ////cerr << "@MAP LEN" << header.v_long << endl;

    //READING @ KEY
    element.pos += read_type_value(src, element.pos, size, &header);
    uint32_t    ep = (uint32_t)(element.pos + header.v_long);
    std::string str(src + element.pos, header.v_long);
    element.pos = ep;
    ////cerr << "@STR " << str << endl;

    //READING URI
    element.pos += read_type_value(src, element.pos, size, &header);
    ep           = (uint32_t)(element.pos + header.v_long);
    std::string uri(src + element.pos, header.v_long);
    js_map->Set(String::NewFromUtf8(isolate, "@"), String::NewFromUtf8(isolate, uri.c_str()));
    element.pos = ep;
    //cerr << "@URI " << uri << endl;

    for (int i = 1; i < n_resources; i++)
    {
        ElementHeader resource_header;
        element.pos += read_type_value(src, element.pos, size, &resource_header);
        uint32_t      ep = (uint32_t)(element.pos + resource_header.v_long);
        std::string   predicate(src + element.pos, resource_header.v_long);
        element.pos = ep;
        //cerr << "@DECODE PREDICATE" << endl;
        //cerr << "@PREDICATE " << predicate << endl;

        element.pos += read_type_value(src, element.pos, size, &resource_header);

        Handle<Value>         predicate_v8 = String::NewFromUtf8(isolate, predicate.c_str());
        v8::Handle<v8::Array> resources_v8 = v8::Array::New(isolate, 1);
        if (resource_header.type == TEXT_STRING)
        {
            Handle<Object> rr_v8 = Object::New(isolate);

            //cerr << "\t@TEXT STRING" << endl;
            uint32_t    ep = (uint32_t)(element.pos + resource_header.v_long);
            std::string val(src + element.pos, resource_header.v_long);
            rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));
            element.pos = ep;
            //cerr << "\t\t@ VAL " << val << endl;

            if (resource_header.tag == TEXT_RU)
            {
                //cerr << "\t\t@STR RU" << endl;
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));
                rr_v8->Set(f_lang, String::NewFromUtf8(isolate, "RU"));
            }
            else if (resource_header.tag == TEXT_EN)
            {
                //cerr << "\t\t@STR EN" << endl;
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));
                rr_v8->Set(f_lang, String::NewFromUtf8(isolate, "EN"));
            }
            else if (resource_header.tag == URI)
            {
                //cerr << "\t\t@STR URI" << endl;
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Uri"));
            }
            else
            {
                //cerr << "\t\t@STR NONE" << endl;
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));
            }

            resources_v8->Set(0, rr_v8);
        }
        else if (resource_header.type == NEGATIVE_INTEGER || resource_header.type == UNSIGNED_INTEGER)
        {
            //cerr << "\t@0 INT=" << resource_header.v_long << endl;
            Handle<Object> rr_v8 = Object::New(isolate);
            if (resource_header.tag == EPOCH_DATE_TIME)
            {
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Datetime"));
                rr_v8->Set(f_data, v8::Date::New(isolate, resource_header.v_long * 1000));
            }
            else
            {
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Integer"));
                rr_v8->Set(f_data, v8::Integer::New(isolate, resource_header.v_long));
            }
            //cerr << "\t@1 INT=" << resource_header.v_long << endl;
            resources_v8->Set(0, rr_v8);
        }
        else if (resource_header.type == FLOAT_SIMPLE)
        {
            Handle<Object> rr_v8 = Object::New(isolate);
            //cerr << "\t@FLOAT SIMPLE" << endl;
            if (resource_header.v_long == _TRUE)
                rr_v8->Set(f_data, v8::Boolean::New(isolate, true));
            else if (resource_header.v_long == _FALSE)
                rr_v8->Set(f_data, v8::Boolean::New(isolate, false));

            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Boolean"));
            resources_v8->Set(0, rr_v8);
        }
        else if (resource_header.type == ARRAY)
        {
            // element.pos += read_type_value(src, element.pos, size, &resource_header);
            //cerr << "\t@ARRAY" << endl;
            if (resource_header.tag == DECIMAL_FRACTION)
            {
                //cerr << "\t\t@DECIMAL" << endl;

                element.pos += read_type_value(src, element.pos, size, &resource_header);
                long decimal_mantissa_data = resource_header.v_long;
                //cerr << "\t\tmant=" << decimal_mantissa_data;

                element.pos += read_type_value(src, element.pos, size, &resource_header);
                long decimal_exponent_data = resource_header.v_long;
                //cerr << " exp=" << decimal_exponent_data << endl;

                string ss = exponent_and_mantissa_to_string(decimal_mantissa_data, decimal_exponent_data);

                //cerr << " ss=" << ss << endl;

                Handle<Object> rr_v8 = Object::New(isolate);
                rr_v8->Set(f_data, String::NewFromUtf8(isolate, ss.c_str()));
                rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Decimal"));
                resources_v8->Set(0, rr_v8);
            }
            else
            {
                int n_elems = resource_header.v_long;
                // element.pos--;
                //cerr << "\t\t@RESOURCES ARRAY " << n_elems << endl;
                for (int j = 0; j < n_elems; j++)
                {
                    element.pos += read_type_value(src, element.pos, size, &resource_header);
                    ////cerr << "\t\t\tj=" << j << endl;
                    if (resource_header.type == TEXT_STRING)
                    {
                        Handle<Object> rr_v8 = Object::New(isolate);

                        //cerr << "\t\t\t@TEXT STRING" << endl;
                        uint32_t    ep = (uint32_t)(element.pos + resource_header.v_long);
                        std::string val(src + element.pos, resource_header.v_long);
                        rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));
                        element.pos = ep;
                        //cerr << "\t\t\t\t@ VAL " << val << endl;

                        if (resource_header.tag == TEXT_RU)
                        {
                            //cerr << "\t\t@STR RU" << endl;
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));
                            rr_v8->Set(f_lang, String::NewFromUtf8(isolate, "RU"));
                        }
                        else if (resource_header.tag == TEXT_EN)
                        {
                            //cerr << "\t\t@STR EN" << endl;
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));
                            rr_v8->Set(f_lang, String::NewFromUtf8(isolate, "EN"));
                        }
                        else if (resource_header.tag == URI)
                        {
                            //cerr << "\t\t@STR URI" << endl;
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Uri"));
                        }
                        else
                        {
                            //cerr << "\t\t@STR NONE" << endl;
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));
                        }

                        resources_v8->Set(j, rr_v8);
                    }
                    else if (resource_header.type == NEGATIVE_INTEGER || resource_header.type == UNSIGNED_INTEGER)
                    {
                        Handle<Object> rr_v8 = Object::New(isolate);
                        if (resource_header.tag == EPOCH_DATE_TIME)
                        {
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Datetime"));
                            rr_v8->Set(f_data, v8::Date::New(isolate, resource_header.v_long * 1000));
                        }
                        else
                        {
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Integer"));
                            rr_v8->Set(f_data, v8::Integer::New(isolate, resource_header.v_long));
                        }
                        //cerr << "\t\t\t@INT=" << resource_header.v_long << endl;
                        resources_v8->Set(j, rr_v8);
                    }
                    else if (resource_header.type == FLOAT_SIMPLE)
                    {
                        Handle<Object> rr_v8 = Object::New(isolate);
                        //cerr << "\t\t\t@FLOAT SIMPLE" << endl;
                        if (resource_header.v_long == _TRUE)
                            rr_v8->Set(f_data, v8::Boolean::New(isolate, true));
                        else if (resource_header.v_long == _FALSE)
                            rr_v8->Set(f_data, v8::Boolean::New(isolate, false));

                        rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Boolean"));
                        resources_v8->Set(j, rr_v8);
                    }
                    else if (resource_header.type == ARRAY)
                    {
                        // element.pos += read_type_value(src, element.pos, size, &resource_header);
                        //cerr << "\t@ARRAY" << endl;
                        if (resource_header.tag == DECIMAL_FRACTION)
                        {
                            //cerr << "\t\t@DECIMAL" << endl;

                            element.pos += read_type_value(src, element.pos, size, &resource_header);
                            long decimal_mantissa_data = resource_header.v_long;
                            //cerr << "\t\tmant=" << resource_header.v_long;

                            element.pos += read_type_value(src, element.pos, size, &resource_header);
                            long decimal_exponent_data = resource_header.v_long;
                            ////cerr << " exp=" << resource_header.v_long << endl;

                            string         ss = exponent_and_mantissa_to_string(decimal_mantissa_data, decimal_exponent_data);

                            Handle<Object> rr_v8 = Object::New(isolate);
                            rr_v8->Set(f_data, String::NewFromUtf8(isolate, ss.c_str()));
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Decimal"));
                            resources_v8->Set(j, rr_v8);
                        }
                    }
                }
            }
        }

        //cerr << "@SAVING TO RESOURCES" << endl;
        if (resources_v8->Length() > 0)
        {
            //cerr << "\t@GRATER THAN ZERO" << endl;
            js_map->Set(predicate_v8, resources_v8);
        }
        //cerr << "@SAVED TO RESOURCES" << endl;
    }

    //cerr << "@FINISH CBOR 2 JS" << endl;
    return js_map;
}

void double_to_mantissa_exponent(double inp, int64_t *mantissa, int64_t *exponent)
{
    double a     = trunc(inp);
    int    power = 0;

    while (inp - trunc(inp) != 0.0)
    {
        inp *= 10.0;
        power--;
    }

    *mantissa = (int64_t)inp;
    *exponent = power;

    //std::cerr << "@c double_to_mantissa_exponent inp=" << inp << ", mantissa=" << *mantissa << ", exponent=" << *exponent << std::endl;
}

bool
jsobject_log(Local<Value> value)
{
    cerr << "!!START LOGGING!!" << endl;
    cerr << "@IS OBJECT " << value->IsObject() << endl;
    Local<Object>         obj = Local<Object>::Cast(value);

    v8::Handle<v8::Array> individual_keys = obj->GetPropertyNames();

    bool                  is_individual_value = false;
    bool                  is_lang_set         = false;

    uint32_t              length = individual_keys->Length();
    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value> js_key        = individual_keys->Get(i);
        std::string          resource_name = std::string(*v8::String::Utf8Value(js_key));
        cerr << "@RESOURCE KEY " << resource_name << endl;

        Local<Value> js_value = obj->Get(js_key);

        if (resource_name == "@")
        {
            std::string uri = std::string(*v8::String::Utf8Value(js_value));
            cerr << "\t@URI DATA " << uri << endl;
            continue;
        }

        cerr << "\t@IS ARRAY " << js_value->IsArray() << endl;
        Local<v8::Array> resources_arr    = Local<v8::Array>::Cast(js_value);
        uint32_t         resources_length = resources_arr->Length();
        cerr << "\t@LENGTH " << resources_length << endl;
        for (uint32_t j = 0; j < resources_length; j++)
        {
            js_value = resources_arr->Get(j);


            if (js_value->IsObject())
            {
                Local<Object>         resource_obj = Local<Object>::Cast(js_value);

                v8::Handle<v8::Array> resource_keys   = resource_obj->GetPropertyNames();
                uint32_t              resource_length = individual_keys->Length();
                // jsobject2individual(js_value, indv, resource, predicate);
                for (uint32_t k = 0; k < resource_length; k++)
                {
                    Local<Value> v_data;
                    Local<Value> v_lang;
                    Local<Value> v_type;
                    js_key = resource_keys->Get(k);
                    std::string  element_name = std::string(*v8::String::Utf8Value(js_key));

                    if (element_name == "data")
                    {
                        cerr << "\t\t\t@ELEMENT KEY " << element_name << endl;
                        // это поле для модели индивида в js
                        v_data = resource_obj->Get(js_key);
                        if (v_data->IsString())
                        {
                            std::string str_data = std::string(*v8::String::Utf8Value(v_data));
                            cerr << "\t\t\t\t@STR DATA " << str_data << endl;
                        }
                    }
                    else if (element_name == "type")
                    {
                        cerr << "\t\t\t@ELEMENT KEY " << element_name << endl;
                        // это поле для модели индивида в js
                        v_type = resource_obj->Get(js_key);
                        cerr << "\t\t\t\t@TYPE " << v_type->ToInteger()->Value() << endl;
                    }
                    else if (element_name == "lang")
                    {
                        cerr << "\t\t\t@ELEMENT KEY " << element_name << endl;
                        // это поле для модели индивида в js
                        v_lang = resource_obj->Get(js_key);
                        cerr << "\t\t\t\t@TYPE " << v_lang->ToInteger()->Value() << endl;
                    }
                }
            }
        }
    }

    cerr << "!!END LOGGING!!" << endl;
    return true;
}

void prepare_js_object(Local<Object> resource_obj, Handle<Value>         f_data, Handle<Value>         f_type,
                       Handle<Value>         f_lang,
                       std::vector<char> &ou)
{
    v8::Handle<v8::Array> resource_keys = resource_obj->GetPropertyNames();
    Local<Value>          v_data        = resource_obj->Get(f_data);
    Local<Value>          v_type        = resource_obj->Get(f_type);

    int                   type = 2;

    if (v_type->IsString())
    {
        string s_type = std::string(*v8::String::Utf8Value(v_type));

        if (s_type.compare("Uri") == 0)
            type = 1;
        else if (s_type.compare("String") == 0)
            type = 2;
        else if (s_type.compare("Integer") == 0)
            type = 4;
        else if (s_type.compare("Datetime") == 0)
            type = 8;
        else if (s_type.compare("Decimal") == 0)
            type = 32;
        else if (s_type.compare("Boolean") == 0)
            type = 64;
    }
    else
        type = v_type->ToInteger()->Value();

    //cerr << "\t\t@TYPE " << type << endl;
    if (type == _Uri)
    {
        string str_data = std::string(*v8::String::Utf8Value(v_data));
        write_type_value(TAG, URI, ou);
        write_string(str_data, ou);
        //cerr << "\t\t\t@STR DATA " << str_data << endl;
    }
    else if (type == _Boolean)
    {
        bool bool_data = v_data->ToBoolean()->Value();
        write_bool(bool_data, ou);
        //cerr << "\t\t\t@BOOL DATA " << bool_data << endl;
    }
    else if (type == _Datetime)
    {
        int64_t long_data = v_data->ToInteger()->Value() / 1000;
        write_type_value(TAG, EPOCH_DATE_TIME, ou);
        write_integer(long_data, ou);
        //cerr << "\t\t\t@DATETIME DATA " << long_data << endl;
    }
    else if (type == _Integer)
    {
        int64_t long_data = v_data->ToInteger()->Value();
        write_integer(long_data, ou);
        //cerr << "\t\t\t@LONG DATA " << long_data << endl;
    }
    else if (type == _Decimal)
    {
        int64_t decimal_mantissa_data, decimal_exponent_data;
        if (v_data->IsString())
        {
            v8::String::Utf8Value s1_1(v_data);
            std::string           num = std::string(*s1_1);
            //std::cerr << "@jsobject2individual value=" << num << std::endl;

            int pos = num.find('.');
            if (pos < 0)
                pos = num.find(',');

            //std::cerr << "@pos=" << pos << std::endl;

            if (pos > 0)
            {
                string ll = num.substr(0, pos);
                string rr = num.substr(pos + 1, num.length());

                size_t sfp = rr.length();

                decimal_mantissa_data = stol(ll + rr);
                decimal_exponent_data = -sfp;
            }
            else
            {
                decimal_mantissa_data = stol(num);
                decimal_exponent_data = 0;
            }
        }
        else
        {
            double dd = v_data->ToNumber()->Value();
            double_to_mantissa_exponent(dd, &decimal_mantissa_data, &decimal_exponent_data);
        }

        write_type_value(TAG, DECIMAL_FRACTION, ou);
        write_type_value(ARRAY, 2, ou);
        write_integer(decimal_mantissa_data, ou);
        write_integer(decimal_exponent_data, ou);
        //cerr << "\t\t\t@DECIMAL DATA " << "MANT=" << decimal_mantissa_data << " EXP=" << decimal_exponent_data << endl;
    }
    else if (type == _String)
    {
        Local<Value> v_lang = resource_obj->Get(f_lang);
        int          lang;

        if (v_lang->IsString())
        {
            string s_lang = std::string(*v8::String::Utf8Value(v_lang));
            std::transform(s_lang.begin(), s_lang.end(), s_lang.begin(), ::toupper);

            if (s_lang.compare("RU") == 0)
                lang = LANG_RU;
            else if (s_lang.compare("EN") == 0)
                lang = LANG_EN;
            else
                lang = LANG_NONE;
        }
        else
        {
            lang = v_lang->ToInteger()->Value();
        }

        string str_data = std::string(*v8::String::Utf8Value(v_data));
        if (lang != LANG_NONE)
            write_type_value(TAG, lang + 41, ou);
        write_string(str_data, ou);

        //cerr << "@STR DATA " << str_data << "LANG: " << lang << endl;
    }
}

void
jsobject2cbor(Local<Value> value, Isolate *isolate, std::vector<char> &ou)
{
    //cerr <<"!!START LOGGING!!" << endl;

    //jsobject_log(value);

    //cerr << "@IS OBJECT " << value->IsObject() << endl;
    Local<Object>         obj = Local<Object>::Cast(value);

    v8::Handle<v8::Array> individual_keys = obj->GetPropertyNames();
    Handle<Value>         f_data          = String::NewFromUtf8(isolate, "data");
    Handle<Value>         f_type          = String::NewFromUtf8(isolate, "type");
    Handle<Value>         f_lang          = String::NewFromUtf8(isolate, "lang");

    uint32_t              length = individual_keys->Length();
    MajorType             type   = MAP;
    write_type_value(type, length, ou);
    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value> js_key        = individual_keys->Get(i);
        std::string          resource_name = std::string(*v8::String::Utf8Value(js_key));
        //cerr << "@RESOURCE KEY " << resource_name << endl;
        Local<Value>         js_value = obj->Get(js_key);
        if (resource_name == "@")
        {
            write_string(resource_name, ou);
            std::string uri = std::string(*v8::String::Utf8Value(js_value));
            write_string(uri, ou);
            //cerr << "\t@URI DATA " << uri << endl;
            continue;
        }

        write_string(resource_name, ou);

        if (!js_value->IsArray())
        {
            if (js_value->IsObject())
            {
//              {}
                write_type_value(ARRAY, 1, ou);
                Local<Object> resource_obj = Local<Object>::Cast(js_value);
                prepare_js_object(resource_obj, f_data, f_type, f_lang, ou);
            }
            else
            {
                write_type_value(ARRAY, 0, ou);
            }
            continue;
        }


        //cerr << "\t@IS ARRAY " << js_value->IsArray() << endl;
        Local<v8::Array> resources_arr    = Local<v8::Array>::Cast(js_value);
        uint32_t         resources_length = resources_arr->Length();
        //cerr << "\t@LENGTH " << resources_length << endl;
        write_type_value(ARRAY, resources_length, ou);

        for (uint32_t j = 0; j < resources_length; j++)
        {
            js_value = resources_arr->Get(j);

            if (js_value->IsArray())
            {
                //cerr << "[ [ {} ] ]" << endl;
//             [ [ {} ] ]

                Local<v8::Array> resources_in_arr    = Local<v8::Array>::Cast(js_value);
                uint32_t         resources_in_length = resources_in_arr->Length();

                if (resources_in_length == 1)
                {
                    js_value = resources_in_arr->Get(0);

                    Local<Object> resource_obj = Local<Object>::Cast(js_value);
                    prepare_js_object(resource_obj, f_data, f_type, f_lang, ou);
                }
                else
                {
                    //cerr << "[ [ {} ], [ {} ] ]" << endl;
//                  [ [ {} ], [ {} ] ]
                    cerr << "ERR! INVALID JS INDIVIDUAL FORMAT " << endl;
                    jsobject_log(value);
                }
            }
            else
            {
                if (js_value->IsObject())
                {
//             [ {} ]
                    //cerr << "[ {} ]" << endl;
                    Local<Object> resource_obj = Local<Object>::Cast(js_value);
                    prepare_js_object(resource_obj, f_data, f_type, f_lang, ou);
                }
                else
                {
                    cerr << "ERR! INVALID JS INDIVIDUAL FORMAT, NULL VALUE, " << endl;
                    jsobject_log(value);

                    write_type_value(ARRAY, 0, ou);
                }
            }
        }
    }

    //cerr << "!!END LOGGING!!" << endl;
}

///////pacahon IO section //////////////////////////////////////////////////////////////////////////////////////////////////

struct _Buff
{
    char *data;
    int  length;
    int  allocated_size;
};

_Buff *get_from_ght(const char *name, int name_length);
void put_to_ght(const char *name, int name_length, const char *value, int value_length);
_Buff *uris_pop(const char *consumer_id, int consumer_id_length);
_Buff *new_uris_consumer();
bool uris_commit_and_next(const char *_consumer_id, int _consumer_id_length, bool is_sync_data);

_Buff *
get_env_str_var(const char *_var_name, int _var_name_length);

_Buff *
query(const char *_ticket, int _ticket_length, const char *_query, int _query_length,
      const char *_sort, int _sort_length, const char *_databases, int _databases_length, int top, int limit);

_Buff *
read_individual(const char *_ticket, int _ticket_length, const char *_uri, int _uri_length);
int
put_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
               int _event_id_length);
int
remove_individual(const char *_ticket, int _ticket_length, const char *_uri, int _uri_length, const char *_event_id,
                  int _event_id_length);
int
add_to_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
                  int _event_id_length);
int
set_in_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
                  int _event_id_length);
int
remove_from_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length,
                       const char *_event_id, int _event_id_length);

void log_trace(const char *_str, int _str_length);

//char *get_resource (int individual_idx, const char* _uri, int _uri_length, int* count_resources, int resource_idx);

///////////////////////////////////////////////////////////////////////////////////////////////////////////

class WrappedContext
{
public:
    WrappedContext ();
    ~WrappedContext ();

    Persistent<Context> context_;
    Isolate             *isolate_;
    Isolate *
    GetIsolate()
    {
        return isolate_;
    }
};

class WrappedScript
{
public:

    WrappedScript ()
    {
    }
    ~WrappedScript ();

    Persistent<Script> script_;
};

// Extracts a C string from a V8 Utf8Value.
const char *
ToCString(const v8::String::Utf8Value& value)
{
    return *value ? *value : "<string conversion failed>";
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////

void
GetEnvStrVariable(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 1)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value str(args[ 0 ]);
    const char            *var_name = ToCString(str);
    _Buff                 *res      = get_env_str_var(var_name, str.length());

    if (res != NULL)
    {
        std::string data(res->data, res->length);

        //std::cerr << "@c:get #3 " << std::endl;
//        std::cerr << "@c:get #3 [" << vv << "]" << std::endl;
        Handle<Value> oo = String::NewFromUtf8(isolate, data.c_str());
        args.GetReturnValue().Set(oo);
    }
}

std::string prepare_str_list_element(std::string data, std::string::size_type b_p, std::string::size_type e_p)
{
    while (data.at(b_p) == ' ')
        b_p++;

    while (data.at(b_p) == '"')
        b_p++;

    while (data.at(e_p - 1) == ' ')
        e_p--;

    while (data.at(e_p - 1) == '"')
        e_p--;

    std::string substring(data.substr(b_p, e_p - b_p));

    //std::cerr << "@c:query, ss= " << substring << std::endl;

    return substring;
}

void
Query(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() < 2)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value _ticket(args[ 0 ]);
    const char            *cticket = ToCString(_ticket);

    v8::String::Utf8Value _query(args[ 1 ]);
    if (_query.length() == 0)
        return;

    const char *cquery = ToCString(_query);

    const char *csort      = NULL;
    const char *cdatabases = NULL;

    int        sort_len      = 0;
    int        databases_len = 0;

    if (args.Length() > 2)
    {
        v8::String::Utf8Value _sort(args[ 2 ]);
        if (_sort.length() > 1)
        {
            csort    = ToCString(_sort);
            sort_len = _sort.length();
        }

        if (args.Length() > 3)
        {
            v8::String::Utf8Value _databases(args[ 3 ]);
            if (_databases.length() > 1)
            {
                cdatabases    = ToCString(_databases);
                databases_len = _databases.length();
            }
        }
    }

    int                   top   = 100000;
    int                   limit = 100000;

    _Buff                 *res  = query(cticket, _ticket.length(), cquery, _query.length(), csort, sort_len, cdatabases, databases_len, top, limit);
    v8::Handle<v8::Array> arr_1 = v8::Array::New(isolate, 0);

    if (res != NULL)
    {
        std::string data(res->data, res->length);

        if (data.length() > 5)
        {
            std::string::size_type prev_pos = 1, pos = 1;
            std::string            el;

            int                    i = 0;
            while ((pos = data.find(',', pos)) != std::string::npos)
            {
                el = prepare_str_list_element(data, prev_pos, pos);
                if (el.length() > 2)
                {
                    arr_1->Set(i, String::NewFromUtf8(isolate, el.c_str()));
                    i++;
                }
                prev_pos = ++pos;
            }
            el = prepare_str_list_element(data, prev_pos, data.length() - 1);

            if (el.length() > 2)
            {
                arr_1->Set(i, String::NewFromUtf8(isolate, el.c_str()));
            }
        }
    }
    args.GetReturnValue().Set(arr_1);
}

////////////////

void
NewUrisConsumer(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 0)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    _Buff *res = new_uris_consumer();
    if (res != NULL)
    {
        std::string   data(res->data, res->length);
        Handle<Value> oo = String::NewFromUtf8(isolate, data.c_str());
        args.GetReturnValue().Set(oo);
    }
}

void
UrisPop(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 1)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "uris_pop: bad parameters"));
        return;
    }

    v8::String::Utf8Value _id(args[ 0 ]);
    const char            *cid = ToCString(_id);

    _Buff                 *res = uris_pop(cid, _id.length());

    if (res != NULL)
    {
        std::string   data(res->data, res->length);

        Handle<Value> oo = String::NewFromUtf8(isolate, data.c_str());

        args.GetReturnValue().Set(oo);
    }
}

void
UrisCommitAndNext(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 2)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "commit_and_next: bad parameters"));
        return;
    }

    v8::String::Utf8Value _id(args[ 0 ]);
    const char            *cid = ToCString(_id);

    bool                  is_sync_data = args[ 1 ]->BooleanValue();

    bool                  res = uris_commit_and_next(cid, _id.length(), is_sync_data);

    args.GetReturnValue().Set(res);
}

/////////////////////
void GetFromGHT(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 1)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value _name(args[ 0 ]);
    const char            *cname = ToCString(_name);

    _Buff                 *res = get_from_ght(cname, _name.length());
    if (res != NULL)
    {
        std::string   data(res->data, res->length);
        Handle<Value> oo = String::NewFromUtf8(isolate, data.c_str());
        args.GetReturnValue().Set(oo);
    }
}

void PutToGHT(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 2)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value _name(args[ 0 ]);
    const char            *cname = ToCString(_name);

    v8::String::Utf8Value _value(args[ 1 ]);
    const char            *cvalue = ToCString(_value);

    put_to_ght(cname, _name.length(), cvalue, _value.length());
}

////////////////////

void
GetIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    // cerr << "#START GETINDIVIDUAL#" << endl;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 2)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value str(args[ 0 ]);
    const char            *ticket = ToCString(str);

    v8::String::Utf8Value str1(args[ 1 ]);

    if (str1.length() == 0)
        return;

    const char *cstr = ToCString(str1);

    _Buff      *doc_as_cbor = read_individual(ticket, str.length(), cstr, str1.length());

    if (doc_as_cbor != NULL)
    {
        std::string data(doc_as_cbor->data, doc_as_cbor->length);

        //std::cerr << "@c #get_individual uri=" << cstr << std::endl;

        // Handle<Value> oo = individual2jsobject(&individual, isolate);
        Handle<Value> oo = cbor2jsobject(isolate, data);
        // jsobject_log(oo, &individual, NULL, "");

        //std::cerr << "@c #get_individual #E" << std::endl;
        args.GetReturnValue().Set(oo);
    }

    // cerr << "#END GET INDIVIDUAL#" << endl;
}

void
RemoveIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int     res      = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "RemoveIndividual::Bad count parameters"));

        return;
    }

    v8::String::Utf8Value str(args[ 0 ]);
    const char            *ticket = ToCString(str);

    v8::String::Utf8Value str1(args[ 1 ]);

    if (str1.length() == 0)
        return;

    const char            *cstr = ToCString(str1);

    v8::String::Utf8Value str_event_id(args[ 2 ]);
    const char            *event_id = ToCString(str_event_id);

    res = remove_individual(ticket, str.length(), cstr, str1.length(), event_id, str_event_id.length());

    args.GetReturnValue().Set(res);
}

void
PutIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int     res      = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        string jsnstr = json_str(isolate, args[ 1 ]);
        //std::cerr << "@c #put_individual json=" << jsnstr << std::endl;

        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char>     buff;
        // std::vector<char>     buff1;

        jsobject2cbor(args[ 1 ], isolate, buff);
        // cerr << "@ORIG " << buff.size() << "[" << std::string(buff.data(), buff.size()) << "]" << endl;
        // cerr << "@NEW " << buff1.size() << "[" << std::string(buff1.data(), buff1.size()) << "]" << endl;

        char *ptr = buff.data();
        res = put_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

void
AddToIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int     res      = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char>     buff;
        // std::vector<char>     buff1;

        jsobject2cbor(args[ 1 ], isolate, buff);
        // cerr << "@ORIG " << buff.size() << "[" << std::string(buff.data(), buff.size()) << "]" << endl;
        // cerr << "@NEW " << buff1.size() << "[" << std::string(buff1.data(), buff1.size()) << "]" << endl;

        jsobject2cbor(args[ 1 ], isolate, buff);
        char *ptr = buff.data();
        res = add_to_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

void
SetInIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int     res      = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char>     buff;
        // std::vector<char>     buff1;

        jsobject2cbor(args[ 1 ], isolate, buff);
        // cerr << "@ORIG " << buff.size() << "[" << std::string(buff.data(), buff.size()) << "]" << endl;
        // cerr << "@NEW " << buff1.size() << "[" << std::string(buff1.data(), buff1.size()) << "]" << endl;

        jsobject2cbor(args[ 1 ], isolate, buff);
        char *ptr = buff.data();
        res = set_in_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

void
RemoveFromIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int     res      = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char>     buff;
        // std::vector<char>     buff1;

        jsobject2cbor(args[ 1 ], isolate, buff);
        // cerr << "@ORIG " << buff.size() << "[" << std::string(buff.data(), buff.size()) << "]" << endl;
        // cerr << "@NEW " << buff1.size() << "[" << std::string(buff1.data(), buff1.size()) << "]" << endl;

        char *ptr = buff.data();
        res = remove_from_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

// The callback that is invoked by v8 whenever the JavaScript 'print'
// function is called.  Prints its arguments on stdout separated by
// spaces and ending with a newline.
void
Print(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    bool            first = true;
    v8::HandleScope handle_scope(args.GetIsolate());

    if (args.Length() == 0)
        return;

    v8::String::Utf8Value str(args[ 0 ]);
    const char            *cstr = ToCString(str);
    std::string           sstr(cstr, str.length());

    if (args.Length() > 1)
    {
        for (int i = 1; i < args.Length(); i++)
        {
            sstr = sstr + " ";

            v8::String::Utf8Value str_i(args[ i ]);
            const char            *cstr_i = ToCString(str_i);
            std::string           sstr_i(cstr_i, str_i.length());
            sstr = sstr + sstr_i;
        }
    }

    log_trace(sstr.c_str(), sstr.length());
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////

WrappedContext::WrappedContext ()
{
    isolate_ = v8::Isolate::New();

    v8::Locker         locker(isolate_);
    v8::Isolate::Scope isolateScope(isolate_);
    HandleScope        handle_scope(isolate_);

    // Create a template for the global object.
    v8::Handle<v8::ObjectTemplate> global = v8::ObjectTemplate::New(isolate_);
    // Bind the global 'print' function to the C++ Print callback.
    global->Set(v8::String::NewFromUtf8(isolate_, "print"), v8::FunctionTemplate::New(isolate_, Print));
    global->Set(v8::String::NewFromUtf8(isolate_, "log_trace"), v8::FunctionTemplate::New(isolate_, Print));

    global->Set(v8::String::NewFromUtf8(isolate_, "get_env_str_var"),
                v8::FunctionTemplate::New(isolate_, GetEnvStrVariable));

    global->Set(v8::String::NewFromUtf8(isolate_, "query"),
                v8::FunctionTemplate::New(isolate_, Query));
    global->Set(v8::String::NewFromUtf8(isolate_, "get_individual"),
                v8::FunctionTemplate::New(isolate_, GetIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "remove_individual"),
                v8::FunctionTemplate::New(isolate_, RemoveIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "put_individual"),
                v8::FunctionTemplate::New(isolate_, PutIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "add_to_individual"),
                v8::FunctionTemplate::New(isolate_, AddToIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "set_in_individual"),
                v8::FunctionTemplate::New(isolate_, SetInIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "remove_from_individual"),
                v8::FunctionTemplate::New(isolate_, RemoveFromIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "uris_pop"),
                v8::FunctionTemplate::New(isolate_, UrisPop));
    global->Set(v8::String::NewFromUtf8(isolate_, "new_uris_consumer"),
                v8::FunctionTemplate::New(isolate_, NewUrisConsumer));
    global->Set(v8::String::NewFromUtf8(isolate_, "put_to_ght"),
                v8::FunctionTemplate::New(isolate_, PutToGHT));
    global->Set(v8::String::NewFromUtf8(isolate_, "get_from_ght"),
                v8::FunctionTemplate::New(isolate_, GetFromGHT));
    global->Set(v8::String::NewFromUtf8(isolate_, "uris_commit_and_next"),
                v8::FunctionTemplate::New(isolate_, UrisCommitAndNext));

    v8::Handle<v8::Context> context = v8::Context::New(isolate_, NULL, global);
    context_.Reset(isolate_, context);
}

WrappedContext::~WrappedContext ()
{
//  context_.Dispose();
}

WrappedScript::~WrappedScript ()
{
//  script_.Dispose();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

WrappedContext *
new_WrappedContext()
{
    WrappedContext *t = new WrappedContext();

    return t;
}

WrappedScript *
new_WrappedScript(WrappedContext *_context, char *src)
{
    Isolate                *isolate = _context->isolate_;
    v8::Locker             locker(isolate);
    v8::Isolate::Scope     isolateScope(isolate);
    HandleScope            scope(isolate);

    v8::Local<v8::Context> context = v8::Local<v8::Context>::New(isolate, _context->context_);
    Context::Scope         context_scope(context);

    Handle<String>         source = v8::String::NewFromUtf8(isolate, src);

    Handle<Script>         sc = Script::Compile(source);

    WrappedScript          *v8ws = new WrappedScript();
    v8ws->script_.Reset(isolate, sc);

    return v8ws;
}

void
run_WrappedScript(WrappedContext *_context, WrappedScript *ws, _Buff *_res, _Buff *_out)
{
    Isolate                *isolate = _context->isolate_;

    v8::Locker             locker(isolate);
    v8::Isolate::Scope     isolateScope(isolate);

    HandleScope            scope(isolate);

    v8::Local<v8::Context> context = v8::Local<v8::Context>::New(isolate, _context->context_);
    Context::Scope         context_scope(context);

    v8::Local<v8::Script>  script = v8::Local<v8::Script>::New(isolate, ws->script_);

    v8::V8::SetFatalErrorHandler(FatalErrorCallback_r);
    Handle<Value> result = script->Run();

    if (_res != NULL)
    {
        String::Utf8Value utf8(result);

        int               c_length;

        if (utf8.length() >= _res->allocated_size)
            c_length = _res->allocated_size;
        else
            c_length = utf8.length();

        memcpy(_res->data, *utf8, c_length);
        _res->length = c_length;
    }

//    printf("Script result: %s\n", *utf8);

//  bool finished = false;
//  for (int i = 0; i < 200 && !finished; i++)
//  {
//finished =
    isolate->IdleNotification(1000);
//  }
}

void
InitializeICU()
{
    v8::V8::InitializeICU(NULL);
}

void
ShutdownPlatform()
{
    v8::V8::ShutdownPlatform();
}

void
Dispose()
{
    v8::V8::Dispose();
}
