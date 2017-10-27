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

static std::string to_string(double d)
{
    std::ostringstream oss;

    oss.precision(std::numeric_limits<double>::digits10);
    oss << std::fixed << d;
    std::string str = oss.str();

    // Remove padding
    // This must be done in two steps because of numbers like 700.00
    std::size_t pos1 = str.find_last_not_of("0");
    if (pos1 != std::string::npos)
        str.erase(pos1 + 1);

    std::size_t pos2 = str.find_last_not_of(".");
    if (pos2 != std::string::npos)
        str.erase(pos2 + 1);

    return str;
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

string nullz = "00000000000000000000000000000000";

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
                rr_v8->Set(f_type, Integer::New(isolate, _String));
                rr_v8->Set(f_lang, Integer::New(isolate, 1));
            }
            else if (resource_header.tag == TEXT_EN)
            {
                //cerr << "\t\t@STR EN" << endl;
                rr_v8->Set(f_type, Integer::New(isolate, _String));
                rr_v8->Set(f_lang, Integer::New(isolate, 2));
            }
            else if (resource_header.tag == URI)
            {
                //cerr << "\t\t@STR URI" << endl;
                rr_v8->Set(f_type, Integer::New(isolate, _Uri));
            }
            else
            {
                //cerr << "\t\t@STR NONE" << endl;
                rr_v8->Set(f_type, Integer::New(isolate, _String));
            }

            resources_v8->Set(0, rr_v8);
        }
        else if (resource_header.type == NEGATIVE_INTEGER || resource_header.type == UNSIGNED_INTEGER)
        {
            //cerr << "\t@NEG INT" << endl;
            Handle<Object> rr_v8 = Object::New(isolate);
            if (resource_header.tag == EPOCH_DATE_TIME)
            {
                rr_v8->Set(f_type, Integer::New(isolate, _Datetime));
                rr_v8->Set(f_data, v8::Date::New(isolate, resource_header.v_long * 1000));
            }
            else
            {
                rr_v8->Set(f_type, Integer::New(isolate, _Integer));
                rr_v8->Set(f_data, v8::Integer::New(isolate, resource_header.v_long));
            }
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

            rr_v8->Set(f_type, Integer::New(isolate, _Boolean));
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
                //cerr << "\t\tmant=" << resource_header.v_long;

                element.pos += read_type_value(src, element.pos, size, &resource_header);
                long decimal_exponent_data = resource_header.v_long;
                ////cerr << " exp=" << resource_header.v_long << endl;

                string str_res;
                string sign = "";
                string str_mantissa;

                if (decimal_mantissa_data < 0)
                {
                    sign         = "-";
                    str_mantissa = to_string(decimal_mantissa_data);
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

                string         ss = sign + slh + "." + slr;

                Handle<Object> rr_v8 = Object::New(isolate);
                rr_v8->Set(f_data, String::NewFromUtf8(isolate, ss.c_str()));
                rr_v8->Set(f_type, Integer::New(isolate, _Decimal));
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
                            rr_v8->Set(f_type, Integer::New(isolate, _String));
                            rr_v8->Set(f_lang, Integer::New(isolate, 1));
                        }
                        else if (resource_header.tag == TEXT_EN)
                        {
                            //cerr << "\t\t@STR EN" << endl;
                            rr_v8->Set(f_type, Integer::New(isolate, _String));
                            rr_v8->Set(f_lang, Integer::New(isolate, 2));
                        }
                        else if (resource_header.tag == URI)
                        {
                            //cerr << "\t\t@STR URI" << endl;
                            rr_v8->Set(f_type, Integer::New(isolate, _Uri));
                        }
                        else
                        {
                            //cerr << "\t\t@STR NONE" << endl;
                            rr_v8->Set(f_type, Integer::New(isolate, _String));
                        }


                        resources_v8->Set(j, rr_v8);
                    }
                    else if (resource_header.type == NEGATIVE_INTEGER || resource_header.type == UNSIGNED_INTEGER)
                    {
                        //cerr << "\t\t\t@NEG INT" << endl;
                        Handle<Object> rr_v8 = Object::New(isolate);
                        if (resource_header.tag == EPOCH_DATE_TIME)
                        {
                            rr_v8->Set(f_type, Integer::New(isolate, _Datetime));
                            rr_v8->Set(f_data, v8::Date::New(isolate, resource_header.v_long * 1000));
                        }
                        else
                        {
                            rr_v8->Set(f_type, Integer::New(isolate, _Integer));
                            rr_v8->Set(f_data, v8::Integer::New(isolate, resource_header.v_long));
                        }
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

                        rr_v8->Set(f_type, Integer::New(isolate, _Boolean));
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

                            string str_res;
                            string sign = "";
                            string str_mantissa;

                            if (decimal_mantissa_data < 0)
                            {
                                sign         = "-";
                                str_mantissa = to_string(decimal_mantissa_data);
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

                            string         ss = sign + slh + "." + slr;

                            Handle<Object> rr_v8 = Object::New(isolate);
                            rr_v8->Set(f_data, String::NewFromUtf8(isolate, ss.c_str()));
                            rr_v8->Set(f_type, Integer::New(isolate, _Decimal));
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
    cerr <<"!!START LOGGING!!" << endl;
    cerr << "@IS OBJECT " << value->IsObject() << endl;
    Local<Object>         obj = Local<Object>::Cast(value);

    v8::Handle<v8::Array> individual_keys = obj->GetPropertyNames();

    bool                  is_individual_value = false;
    bool                  is_lang_set = false;

    uint32_t length = individual_keys->Length();
    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value>  js_key = individual_keys->Get(i);
        std::string           resource_name = std::string(*v8::String::Utf8Value(js_key));
        cerr << "@RESOURCE KEY " << resource_name << endl;

        Local<Value> js_value = obj->Get(js_key);

        if (resource_name == "@") {
            std::string uri = std::string(*v8::String::Utf8Value(js_value));
            cerr << "\t@URI DATA " << uri << endl;
            continue;
        }

        cerr << "\t@IS ARRAY " << js_value->IsArray() << endl;
        Local<v8::Array> resources_arr = Local<v8::Array>::Cast(js_value);
        uint32_t resources_length = resources_arr->Length();
        cerr << "\t@LENGTH " << resources_length << endl;
        for (uint32_t j = 0; j < resources_length; j++)
        {
            js_value = resources_arr->Get(j);
            cerr << "\t\t@IS OBJECT " << js_value->IsObject() << endl;
            Local<Object>         resource_obj = Local<Object>::Cast(js_value);

            v8::Handle<v8::Array> resource_keys = resource_obj->GetPropertyNames();
            uint32_t resource_length = individual_keys->Length();
            // jsobject2individual(js_value, indv, resource, predicate);
            for (uint32_t k = 0; k < resource_length; k++)
            {
                Local<Value>          v_data;
                Local<Value>          v_lang;
                Local<Value>          v_type;
                js_key = resource_keys->Get(k);
                std::string element_name = std::string(*v8::String::Utf8Value(js_key));
                
                if (element_name == "data")
                {
					cerr << "\t\t\t@ELEMENT KEY " << element_name << endl;
                    // это поле для модели индивида в js
                    v_data = resource_obj->Get(js_key);
                    if (v_data->IsString()) {
                        std::string str_data = std::string(*v8::String::Utf8Value(v_data));
                        cerr << "\t\t\t\t@STR DATA " << str_data << endl;
                    }
                }
                else if (element_name == "type")
                {
					cerr << "\t\t\t@ELEMENT KEY " << element_name << endl;
                    // это поле для модели индивида в js
                    v_type              = resource_obj->Get(js_key);
                    cerr << "\t\t\t\t@TYPE " << v_type->ToInteger()->Value() << endl;
                }
                else if (element_name == "lang")
                {
					cerr << "\t\t\t@ELEMENT KEY " << element_name << endl;
                    // это поле для модели индивида в js
                    v_lang              = resource_obj->Get(js_key);
                    cerr << "\t\t\t\t@TYPE " << v_lang->ToInteger()->Value() << endl;
                }
            }
        }
    }

    cerr << "!!END LOGGING!!" << endl;
    return true;
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
    Handle<Value>         f_lang          = String::NewFromUtf8(isolate, "lang");
    Handle<Value>         f_type          = String::NewFromUtf8(isolate, "type");

    uint32_t              length = individual_keys->Length();
    MajorType             type   = MAP;
    write_type_value(type, length, ou);
    for (uint32_t i = 0; i < length; i++)
    {
        v8::Local<v8::Value> js_key        = individual_keys->Get(i);
        std::string          resource_name = std::string(*v8::String::Utf8Value(js_key));
        //cerr << "@RESOURCE KEY " << resource_name << endl;
		Local<Value> js_value = obj->Get(js_key);
        if (resource_name == "@")
        {
			write_string(resource_name, ou);
            std::string uri = std::string(*v8::String::Utf8Value(js_value));
            write_string(uri, ou);
            //cerr << "\t@URI DATA " << uri << endl;
            continue;
        }
        
        write_string(resource_name, ou);
        
        if (!js_value->IsArray()) {
			write_type_value(ARRAY, 0, ou);
			continue;
		}
		
        //cerr << "\t@IS ARRAY " << js_value->IsArray() << endl;
        Local<v8::Array> resources_arr    = Local<v8::Array>::Cast(js_value);
        uint32_t         resources_length = resources_arr->Length();
        //cerr << "\t@LENGTH " << resources_length << endl;
        //if (resources_length > 1)
         write_type_value(ARRAY, resources_length, ou);

        for (uint32_t j = 0; j < resources_length; j++)
        {
            js_value = resources_arr->Get(j);
            Local<Object>         resource_obj = Local<Object>::Cast(js_value);

            v8::Handle<v8::Array> resource_keys   = resource_obj->GetPropertyNames();
            uint32_t              resource_length = individual_keys->Length();
            Local<Value>          v_data = resource_obj->Get(f_data);
            Local<Value>          v_type = resource_obj->Get(f_type);

            int                   type = v_type->ToInteger()->Value();
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
                long long_data = v_data->ToInteger()->Value();
                write_type_value(TAG, EPOCH_DATE_TIME, ou);
                write_integer(long_data, ou);
                //cerr << "\t\t\t@DATETIME DATA " << long_data << endl;
            }
            else if (type == _Integer)
            {
                long long_data = v_data->ToInteger()->Value();
                write_integer(long_data, ou);
                //cerr << "\t\t\t@LONG DATA " << long_data << endl;
            }
            else if (type == _Decimal)
            {
                double  dd = v_data->ToNumber()->Value();
                int64_t decimal_mantissa_data, decimal_exponent_data;
                double_to_mantissa_exponent(dd, &decimal_mantissa_data, &decimal_exponent_data);
                write_type_value(TAG, DECIMAL_FRACTION, ou);
                write_type_value(ARRAY, 2, ou);
                write_integer(decimal_mantissa_data, ou);
                write_integer(decimal_exponent_data, ou);
                //cerr << "\t\t\t@DECIMAL DATA " << "MANT=" << decimal_mantissa_data << " EXP=" << decimal_exponent_data << endl;
            }
            else if (type == _String)
            {
                Local<Value> v_lang   = resource_obj->Get(f_lang);
                int          lang     = v_lang->ToInteger()->Value();
                string       str_data = std::string(*v8::String::Utf8Value(v_data));
                if (lang != LANG_NONE)
                    write_type_value(TAG, lang + 41, ou);
                write_string(str_data, ou);

                //cerr << "@STR DATA " << str_data << "LANG: " << lang << endl;
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
