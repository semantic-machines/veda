#define _GLIBCXX_USE_CXX11_ABI    0

#include <iostream>
#include <algorithm>
#include <msgpack.hpp>
#include "util8json.h"
#include "msgpack8json.h"

using namespace std;
using namespace v8;

//  MSGPACK -> JSON

Handle<Value> msgpack2jsobject(Isolate *isolate, string in_str)
{
    Handle<Object>    js_map = Object::New(isolate);

    Handle<Value>     f_data = String::NewFromUtf8(isolate, "data");
    Handle<Value>     f_lang = String::NewFromUtf8(isolate, "lang");
    Handle<Value>     f_type = String::NewFromUtf8(isolate, "type");
    const char        *src   = in_str.c_str();

    Element           element;

    msgpack::unpacker unpk;

    unpk.reserve_buffer(in_str.length());
    memcpy(unpk.buffer(), in_str.c_str(), in_str.length());
    unpk.buffer_consumed(in_str.length());
    msgpack::object_handle result;
    unpk.next(result);
    msgpack::object        glob_obj(result.get());
    msgpack::object_array  obj_arr = glob_obj.via.array;

    if (obj_arr.size != 2)
        return js_map;

    msgpack::object *obj_uri = obj_arr.ptr;
    msgpack::object *obj_map = obj_arr.ptr + 1;

    string          uri = string(obj_uri->via.str.ptr, obj_uri->via.str.size);

    js_map->Set(String::NewFromUtf8(isolate, "@"), String::NewFromUtf8(isolate, uri.c_str()));

    msgpack::object_map map = obj_map->via.map;
    // std::cerr << "MAP_SIZE " << map.size << endl;

    for (int i = 0; i < map.size; i++)
    {
        // std::cerr << "\tKEY "  << *obj << endl;
        // std::cerr << "\tKEY: " << pair->key << " VALUE: " << pair->val << endl;
        msgpack::object_kv    pair     = map.ptr[ i ];
        msgpack::object       key      = pair.key;
        msgpack::object_array res_objs = pair.val.via.array;
        if (key.type != msgpack::type::STR)
        {
            std::cerr << "@ERR! PREDICATE IS NOT STRING!" << endl;
            return Object::New(isolate);
        }

        std::string           predicate(key.via.str.ptr, key.via.str.size);
        Handle<Value>         predicate_v8 = String::NewFromUtf8(isolate, predicate.c_str());
        v8::Handle<v8::Array> resources_v8 = v8::Array::New(isolate, 1);

        // std::cerr << "SIZE " << res_objs.size << endl;
        for (int j = 0; j < res_objs.size; j++)
        {
            msgpack::object value = res_objs.ptr[ j ];

            switch (value.type)
            {
            case msgpack::type::ARRAY:
                {
                    // std::cerr << "is array" << endl;
                    // std::cerr << "\t\t\tTRY ARR SIZE ";
                    msgpack::object_array res_arr = value.via.array;
                    // std::cerr << "ARR SIZE " << res_arr.size << endl;
                    if (res_arr.size == 2)
                    {
                        long type = res_arr.ptr[ 0 ].via.u64;

                        if (type == _Datetime)
                        {
                            long           value;

                            Handle<Object> rr_v8 = Object::New(isolate);

                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Datetime"));

                            if (res_arr.ptr[ 1 ].type == msgpack::type::POSITIVE_INTEGER)
                                rr_v8->Set(f_data, v8::Date::New(isolate, res_arr.ptr[ 1 ].via.u64 * 1000));
                            else
                                rr_v8->Set(f_data, v8::Date::New(isolate, res_arr.ptr[ 1 ].via.i64 * 1000));

                            resources_v8->Set(j, rr_v8);
                        }
                        else if (type == _String)
                        {
                            Handle<Object> rr_v8 = Object::New(isolate);

                            // std::cerr << "string" << endl;
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));

                            if (res_arr.ptr[ 1 ].type == msgpack::type::STR)
                            {
                                string val = string(res_arr.ptr[ 1 ].via.str.ptr, res_arr.ptr[ 1 ].via.str.size);
                                rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));
                            }
                            else if (res_arr.ptr[ 1 ].type == msgpack::type::NIL)
                            {
                                string val = "";
                                rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));
                            }
                            else
                            {
                                std::cerr << "@ERR! NOT A STRING IN RESOURCE ARRAY 2" << endl;
                                return Object::New(isolate);
                            }

                            resources_v8->Set(j, rr_v8);
                        }
                        else
                        {
                            std::cerr << "@1" << endl;
                            return Object::New(isolate);
                        }
                    }
                    else if (res_arr.size == 3)
                    {
                        long type = res_arr.ptr[ 0 ].via.u64;
                        // std::cerr << "TYPE " << type << endl;
                        if (type == _Decimal)
                        {
                            long mantissa, exponent;
                            // std::cerr << "is decimal" << endl << "\t\t\t\tTRY MANTISSA";
                            if (res_arr.ptr[ 1 ].type == msgpack::type::POSITIVE_INTEGER)
                                mantissa = res_arr.ptr[ 1 ].via.u64;
                            else
                                mantissa = res_arr.ptr[ 1 ].via.i64;
                            // std::cerr << mantissa << endl << "\t\t\t\tTRY EXP";
                            if (res_arr.ptr[ 2 ].type == msgpack::type::POSITIVE_INTEGER)
                                exponent = res_arr.ptr[ 2 ].via.u64;
                            else
                                exponent = res_arr.ptr[ 2 ].via.i64;

                            string ss = exponent_and_mantissa_to_string(mantissa, exponent);

                            // std::cerr << exponent << endl;
                            Handle<Object> rr_v8 = Object::New(isolate);
                            rr_v8->Set(f_data, String::NewFromUtf8(isolate, ss.c_str()));
                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Decimal"));
                            resources_v8->Set(j, rr_v8);
                        }
                        else if (type == _String)
                        {
                            Handle<Object> rr_v8 = Object::New(isolate);

                            rr_v8->Set(f_type, String::NewFromUtf8(isolate, "String"));

                            if (res_arr.ptr[ 1 ].type == msgpack::type::STR)
                            {
                                string val = string(res_arr.ptr[ 1 ].via.str.ptr, res_arr.ptr[ 1 ].via.str.size);
                                rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));
                            }
                            else if (res_arr.ptr[ 1 ].type == msgpack::type::NIL)
                            {
                                string val = "";
                                rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));
                            }
                            else
                            {
                                std::cerr << "@ERR! NOT A STRING IN RESOURCE ARRAY 2" << endl;
                                return Object::New(isolate);
                            }

                            long lang = res_arr.ptr[ 2 ].via.u64;

                            if (lang == LANG_RU)
                                rr_v8->Set(f_lang, String::NewFromUtf8(isolate, "RU"));
                            else if (lang == LANG_EN)
                                rr_v8->Set(f_lang, String::NewFromUtf8(isolate, "EN"));

                            resources_v8->Set(j, rr_v8);
                        }
                        else
                        {
                            std::cerr << "@2" << endl;
                            return Object::New(isolate);
                        }
                    }
                    else
                    {
                        std::cerr << "@3" << endl;
                        return Object::New(isolate);
                    }
                    break;
                }

            case msgpack::type::STR:
                {
                    Handle<Object> rr_v8 = Object::New(isolate);
                    rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Uri"));

                    string val = string(string(value.via.str.ptr, value.via.str.size));
                    rr_v8->Set(f_data, String::NewFromUtf8(isolate, val.c_str()));

                    resources_v8->Set(j, rr_v8);

                    break;
                }

            case msgpack::type::POSITIVE_INTEGER:
                {
                    Handle<Object> rr_v8 = Object::New(isolate);

                    rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Integer"));
                    rr_v8->Set(f_data, v8::Integer::New(isolate, value.via.u64));

                    break;
                }

            case msgpack::type::NEGATIVE_INTEGER:
                {
                    Handle<Object> rr_v8 = Object::New(isolate);
                    rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Integer"));
                    rr_v8->Set(f_data, v8::Integer::New(isolate, value.via.i64));
                    resources_v8->Set(j, rr_v8);

                    break;
                }

            case msgpack::type::BOOLEAN:
                {
                    Handle<Object> rr_v8 = Object::New(isolate);

                    rr_v8->Set(f_type, String::NewFromUtf8(isolate, "Boolean"));
                    rr_v8->Set(f_data, v8::Boolean::New(isolate, value.via.boolean));

                    resources_v8->Set(j, rr_v8);

                    break;
                }

            default:
                {
                    std::cerr << "@ERR! UNSUPPORTED RESOURCE TYPE " << value.type << endl;
                    return Object::New(isolate);
                }
            }
        }
    }

    return js_map;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

//  JSON -> MSGPACK

void js_el_2_msgpack_el(Local<Object> resource_obj, Handle<Value> f_data, Handle<Value> f_type, Handle<Value> f_lang, msgpack::packer<msgpack::sbuffer> &pk)
{
    Local<Value> v_data = resource_obj->Get(f_data);
    Local<Value> v_type = resource_obj->Get(f_type);

    int          type = 2;

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
        pk.pack(str_data);
        //cerr << "\t\t\t@STR DATA " << str_data << endl;
    }
    else if (type == _Boolean)
    {
        bool bool_data = v_data->ToBoolean()->Value();
        pk.pack(bool_data);
        //cerr << "\t\t\t@BOOL DATA " << bool_data << endl;
    }
    else if (type == _Datetime)
    {
        int64_t long_data = v_data->ToInteger()->Value() / 1000;
        pk.pack_array(2);
        pk.pack((uint)_Datetime);
        pk.pack(long_data);
        //cerr << "\t\t\t@DATETIME DATA " << long_data << endl;
    }
    else if (type == _Integer)
    {
        int64_t long_data = v_data->ToInteger()->Value();
        pk.pack(long_data);
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

                decimal_mantissa_data = to_int((ll + rr).c_str());
                decimal_exponent_data = -sfp;
            }
            else
            {
                decimal_mantissa_data = to_int(num.c_str());
                decimal_exponent_data = 0;
            }
        }
        else
        {
            double dd = v_data->ToNumber()->Value();
            double_to_mantissa_exponent(dd, &decimal_mantissa_data, &decimal_exponent_data);
        }

        pk.pack_array(3);
        pk.pack((uint)_Decimal);
        pk.pack(decimal_mantissa_data);
        pk.pack(decimal_exponent_data);
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
        {
            pk.pack_array(3);
            pk.pack((uint)_String);
            pk.pack(str_data);
            pk.pack(lang);
        }
        else
        {
            pk.pack_array(2);
            pk.pack((uint)_String);
            pk.pack(str_data);
        }

        //cerr << "@STR DATA " << str_data << "LANG: " << lang << endl;
    }
}

void jsobject2msgpack(Local<Value> value, Isolate *isolate, std::vector<char> &ou)
{
    //cerr <<"!!START LOGGING!!" << endl;
    //jsobject_log(value);

    //cerr << "@IS OBJECT " << value->IsObject() << endl;
    Local<Object>                     obj = Local<Object>::Cast(value);

    v8::Handle<v8::Array>             individual_keys = obj->GetPropertyNames();
    Handle<Value>                     f_data          = String::NewFromUtf8(isolate, "data");
    Handle<Value>                     f_type          = String::NewFromUtf8(isolate, "type");
    Handle<Value>                     f_lang          = String::NewFromUtf8(isolate, "lang");

    uint32_t                          length = individual_keys->Length();

    msgpack::sbuffer                  buffer;
    msgpack::packer<msgpack::sbuffer> pk(&buffer);

    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value> js_key        = individual_keys->Get(i);
        std::string          resource_name = std::string(*v8::String::Utf8Value(js_key));
        //cerr << "@RESOURCE KEY " << resource_name << endl;
        Local<Value>         js_value = obj->Get(js_key);
        if (resource_name == "@")
        {
            std::string uri = std::string(*v8::String::Utf8Value(js_value));

            pk.pack_array(2);
            pk.pack(uri);
            pk.pack_map(length);
            break;
        }
    }

    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value> js_key        = individual_keys->Get(i);
        std::string          resource_name = std::string(*v8::String::Utf8Value(js_key));
        //cerr << "@RESOURCE KEY " << resource_name << endl;
        Local<Value>         js_value = obj->Get(js_key);
        if (resource_name == "@")
            continue;

        pk.pack(resource_name);

        if (!js_value->IsArray())
        {
            if (js_value->IsObject())
            {
//              {}
                pk.pack_array(1);
                Local<Object> resource_obj = Local<Object>::Cast(js_value);
                js_el_2_msgpack_el(resource_obj, f_data, f_type, f_lang, pk);
            }
            else
            {
                pk.pack_array(0);
            }
            continue;
        }


        //cerr << "\t@IS ARRAY " << js_value->IsArray() << endl;
        Local<v8::Array> resources_arr    = Local<v8::Array>::Cast(js_value);
        uint32_t         resources_length = resources_arr->Length();
        //cerr << "\t@LENGTH " << resources_length << endl;
        pk.pack_array(resources_length);

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
                    js_el_2_msgpack_el(resource_obj, f_data, f_type, f_lang, pk);
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
                    js_el_2_msgpack_el(resource_obj, f_data, f_type, f_lang, pk);
                }
                else
                {
                    //cerr << "ERR! INVALID JS INDIVIDUAL FORMAT, NULL VALUE, " << endl;
                    //jsobject_log(value);

                    pk.pack_array(0);
                }
            }
        }
    }

    ou.insert(ou.end(), buffer.data(), buffer.data() + buffer.size());

    //cerr << "!!END LOGGING!!" << endl;
}
