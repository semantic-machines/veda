#define _GLIBCXX_USE_CXX11_ABI    0

#include <algorithm>
#include "util8json.h"
#include "msgpack8json.h"

using namespace std;
using namespace v8;

//  MSGPACK -> JSON

Handle<Value> msgpack2jsobject(Isolate *isolate, string in_str)
{
    Handle<Object> js_map = Object::New(isolate);

    Handle<Value>  f_data = String::NewFromUtf8(isolate, "data");
    Handle<Value>  f_lang = String::NewFromUtf8(isolate, "lang");
    Handle<Value>  f_type = String::NewFromUtf8(isolate, "type");
    const char     *src   = in_str.c_str();

    //ElementHeader  header;
    Element        element;

    int            size = in_str.size();

    return js_map;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

//  JSON -> MSGPACK

void js_el_2_msgpack_el(Local<Object> resource_obj, Handle<Value> f_data, Handle<Value> f_type, Handle<Value> f_lang, std::vector<char> &ou)
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
        //write_type_value(TAG, URI, ou);
        //write_string(str_data, ou);
        //cerr << "\t\t\t@STR DATA " << str_data << endl;
    }
    else if (type == _Boolean)
    {
        bool bool_data = v_data->ToBoolean()->Value();
        //write_bool(bool_data, ou);
        //cerr << "\t\t\t@BOOL DATA " << bool_data << endl;
    }
    else if (type == _Datetime)
    {
        int64_t long_data = v_data->ToInteger()->Value() / 1000;
        //write_type_value(TAG, EPOCH_DATE_TIME, ou);
        //write_integer(long_data, ou);
        //cerr << "\t\t\t@DATETIME DATA " << long_data << endl;
    }
    else if (type == _Integer)
    {
        int64_t long_data = v_data->ToInteger()->Value();
        //write_integer(long_data, ou);
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

        //write_type_value(TAG, DECIMAL_FRACTION, ou);
        //write_type_value(ARRAY, 2, ou);
        //write_integer(decimal_mantissa_data, ou);
        //write_integer(decimal_exponent_data, ou);
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
        //if (lang != LANG_NONE)
        //    write_type_value(TAG, lang + 41, ou);
        //write_string(str_data, ou);

        //cerr << "@STR DATA " << str_data << "LANG: " << lang << endl;
    }
}

void jsobject2msgpack(Local<Value> value, Isolate *isolate, std::vector<char> &ou)
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
    //MajorType             type   = MAP;
    //write_type_value(type, length, ou);
    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value> js_key        = individual_keys->Get(i);
        std::string          resource_name = std::string(*v8::String::Utf8Value(js_key));
        //cerr << "@RESOURCE KEY " << resource_name << endl;
        Local<Value>         js_value = obj->Get(js_key);
        if (resource_name == "@")
        {
            //write_string(resource_name, ou);
            std::string uri = std::string(*v8::String::Utf8Value(js_value));
            //write_string(uri, ou);
            //cerr << "\t@URI DATA " << uri << endl;
            continue;
        }

        //write_string(resource_name, ou);

        if (!js_value->IsArray())
        {
            if (js_value->IsObject())
            {
//              {}
                //write_type_value(ARRAY, 1, ou);
                Local<Object> resource_obj = Local<Object>::Cast(js_value);
                js_el_2_msgpack_el(resource_obj, f_data, f_type, f_lang, ou);
            }
            else
            {
                //write_type_value(ARRAY, 0, ou);
            }
            continue;
        }


        //cerr << "\t@IS ARRAY " << js_value->IsArray() << endl;
        Local<v8::Array> resources_arr    = Local<v8::Array>::Cast(js_value);
        uint32_t         resources_length = resources_arr->Length();
        //cerr << "\t@LENGTH " << resources_length << endl;
        //write_type_value(ARRAY, resources_length, ou);

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
                    js_el_2_msgpack_el(resource_obj, f_data, f_type, f_lang, ou);
                }
                else
                {
                    //cerr << "[ [ {} ], [ {} ] ]" << endl;
//                  [ [ {} ], [ {} ] ]
                    //cerr << "ERR! INVALID JS INDIVIDUAL FORMAT " << endl;
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
                    js_el_2_msgpack_el(resource_obj, f_data, f_type, f_lang, ou);
                }
                else
                {
                    //cerr << "ERR! INVALID JS INDIVIDUAL FORMAT, NULL VALUE, " << endl;
                    //jsobject_log(value);

                    //write_type_value(ARRAY, 0, ou);
                }
            }
        }
    }

    //cerr << "!!END LOGGING!!" << endl;
}
