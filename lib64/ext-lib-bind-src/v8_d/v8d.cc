#include "v8.h"
#include <assert.h>
#include <iostream>
#include <string>
#include <string.h>
#include "cbor.h"
#include "cbor2individual.h"

using namespace std;
using namespace v8;


//////////////////////////////////////////////////////////////////
Handle<Value> individual2jsobject(Individual *individual, Isolate *isolate)
{
//    std::cout << "@#1" << std::endl;
    Handle<Object>                              js_map = Object::New(isolate);

    Handle<Value>                               f_data = String::NewFromUtf8(isolate, "data");
    Handle<Value>                               f_lang = String::NewFromUtf8(isolate, "lang");
    Handle<Value>                               f_type = String::NewFromUtf8(isolate, "type");

    map < string, vector <Resource> >::iterator p;

    for (p = individual->resources.begin(); p != individual->resources.end(); ++p)
    {
        std::string           key_str = p->first;
        Handle<Value>         key     = String::NewFromUtf8(isolate, key_str.c_str());

        v8::Handle<v8::Array> arr_1 = v8::Array::New(isolate, 1);

        for (int i = 0; i < p->second.size(); i++)
        {
            Handle<Object> in_obj = Object::New(isolate);

            Resource       value = p->second[ i ];
            if (value.type == _String)
            {
                if (value.lang == LANG_RU)
                    in_obj->Set(f_lang, Integer::New(isolate, 1));
                else if (value.lang == LANG_EN)
                    in_obj->Set(f_lang, Integer::New(isolate, 2));


                in_obj->Set(f_data, String::NewFromUtf8(isolate, value.str_data.c_str()));
            }
            else if (value.type == _Decimal)
            {
            }
            else if (value.type == _Datetime)
            {
            }
            else if (value.type == _Boolean)
            {
                in_obj->Set(f_data, v8::Boolean::New(isolate, value.bool_data));
            }
            else if (value.type == _Uri)
            {
                in_obj->Set(f_data, String::NewFromUtf8(isolate, value.str_data.c_str()));
            }


            in_obj->Set(f_type, Integer::New(isolate, value.type));

            arr_1->Set(i, in_obj);
        }

        Handle<Value> js_key = String::NewFromUtf8(isolate, key_str.c_str());
        js_map->Set(js_key, arr_1);
    }

    js_map->Set(String::NewFromUtf8(isolate, "@"), String::NewFromUtf8(isolate, individual->uri.c_str()));

    return js_map;
}


void jsobject2individual(Local<Value> value, Individual *indv, Resource *resource, string predicate)
{
    //std::cout << "@json->cbor #0 predicate=" << predicate << std::endl;

    if (value->IsArray())
    {
        //std::cout << "@json->cbor # is array" << std::endl;
        Local<v8::Array> js_arr = Local<v8::Array>::Cast(value);

        if (js_arr->Length() == 1)
        {
            Local<Value> js_value = js_arr->Get(0);
            jsobject2individual(js_value, indv, resource, predicate);
            return;
        }

        for (uint32_t idx = 0; idx < js_arr->Length(); idx++)
        {
            Local<Value> js_value = js_arr->Get(idx);
            jsobject2individual(js_value, indv, resource, predicate);
        }
        return;
    }
    else if (value->IsString())
    {
        //std::cout << "@json->cbor #is sring" << std::endl;
        v8::String::Utf8Value s1(value);
        std::string           vv = std::string(*s1);

        resource->type     = _String;
        resource->str_data = vv;

        return;
    }
    else if (value->IsBoolean())
    {
        //std::cout << "@json->cbor #is boolean" << std::endl;

        resource->type      = _Boolean;
        resource->bool_data = value->ToBoolean()->Value();

        return;
    }
    else if (value->IsDate())
    {
//        std::cout << "@json->cbor #10" << std::endl;
        resource->type      = _Datetime;
        resource->long_data = value->ToInteger()->Value();

        return;
    }
    else if (value->IsNumber())
    {
        //std::cout << "@json->cbor #11" << std::endl;
    }
    else if (value->IsObject())
    {
        //std::cout << "@json->cbor #is object" << std::endl;
        Local<Object>         obj = Local<Object>::Cast(value);

        v8::Handle<v8::Array> propertyNames = obj->GetPropertyNames();

        bool                  is_individual_value = false;
        Local<Value>          v_data;
        Local<Value>          v_lang;
        Local<Value>          v_type;
        bool is_lang_set = false;

        uint32_t              length = propertyNames->Length();
        for (uint32_t i = 0; i < length; i++)
        {
            v8::Local<v8::Value>  js_key = propertyNames->Get(i);

            v8::String::Utf8Value s1(js_key);
            std::string           name = std::string(*s1);

            //std::cout << "$#1 name=" << name << std::endl;

            if (name == "data")
            {
                // это поле для модели индивида в js
                v_data              = obj->Get(js_key);
                is_individual_value = true;
            }
            else if (name == "type")
            {
                // это поле для модели индивида в js
                v_type              = obj->Get(js_key);
                is_individual_value = true;
            }
            else if (name == "lang")
            {
                // это поле для модели индивида в js
                v_lang              = obj->Get(js_key);
                is_lang_set = true;
                is_individual_value = true;
            }
            else if (name == "@")
            {
                Resource              rc;
                Local<Value>          js_value = obj->Get(js_key);

                v8::String::Utf8Value s2(js_value);
                std::string           vv = std::string(*s2);

                indv->uri = vv;
            }
            else
            {
                Local<Value> js_value = obj->Get(js_key);
                jsobject2individual(js_value, indv, resource, name);
            }
        }

        if (is_individual_value == true)
        {
            //std::cout << "@json->cbor #4" << std::endl;
            int      type = v_type->ToInt32()->Value();

            Resource rc;

            if (type == _Boolean)
            {
                bool              boolValue = v_data->ToBoolean()->Value();
                vector <Resource> values    = indv->resources[ predicate ];
                Resource          rc;
                rc.type      = type;
                rc.bool_data = boolValue;
                values.push_back(rc);
                indv->resources[ predicate ] = values;
                return;
            }
            else if (type == _Datetime)
	    {
//                v8::String::Utf8Value s1_1(v_data);
//                std::string           std_s1_1 = std::string(*s1_1);

                int64_t value = (int64_t)(v_data->ToInteger()->Value()/1000);
//                std::cout << "@json->cbor #5, " << std_s1_1 << ", " << v_data->ToInteger()->Value() << ", " << value << std::endl;
                vector <Resource> values    = indv->resources[ predicate ];
                Resource          rc;
                rc.type      = type;
                rc.long_data = value;
                values.push_back(rc);
                indv->resources[ predicate ] = values;
                return;
	    }	
            else if (type == _Integer)
            {
                int intValue = v_data->ToInteger()->Value();
                return;
            }
            else if (type == _Uri || type == _String)
            {
                Resource rc;

                if (type == _String && is_lang_set == true)
                {
               		int lang = v_lang->ToInt32()->Value();

               		if (lang == 1)
               			rc.lang = LANG_RU;
               		else if (lang == 2)
               			rc.lang = LANG_EN;
                }

                v8::String::Utf8Value s1_1(v_data);
                std::string           std_s1_1 = std::string(*s1_1);

                //std::cout << "@json->cbor #4.1" << std_s1_1 << std::endl;

                vector <Resource> values = indv->resources[ predicate ];
                rc.type     = type;
                rc.str_data = std_s1_1;
                values.push_back(rc);
                indv->resources[ predicate ] = values;
                return;
            }
        }
//        else
//        {
//            return CborValue (map);
//        }
    }

    //std::cout << "@json->cbor #12" << std::endl;
    return;
}

///////pacahon IO section //////////////////////////////////////////////////////////////////////////////////////////////////

struct _Buff
{
    char *data;
    int  length;
    int  allocated_size;
};

char *get_global_prop(const char *prop_name, int prop_name_length);

_Buff *get_env_str_var(const char *_var_name, int _var_name_length);

_Buff *read_individual(const char *_ticket, int _ticket_length, const char *_uri, int _uri_length);
int put_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id, int _event_id_length);
int add_to_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id, int _event_id_length);
int set_in_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id, int _event_id_length);
int remove_from_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id, int _event_id_length);
//char *get_resource (int individual_idx, const char* _uri, int _uri_length, int* count_resources, int resource_idx);

///////////////////////////////////////////////////////////////////////////////////////////////////////////

class WrappedContext
{
public:
    WrappedContext();
    ~WrappedContext();

    Persistent<Context> context_;
    Isolate             *isolate_;
    Isolate *GetIsolate()
    {
        return isolate_;
    }
};

class WrappedScript
{
public:

    WrappedScript()
    {
    }
    ~WrappedScript();

    Persistent<Script> script_;
};

// Extracts a C string from a V8 Utf8Value.
const char *ToCString(const v8::String::Utf8Value& value)
{
    return *value ? *value : "<string conversion failed>";
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////

void GetEnvStrVariable(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 1)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value str(args[ 0 ]);
    const char            *var_name = ToCString(str);
    _Buff                 *res = get_env_str_var(var_name, str.length());

    if (res != NULL)
    {
        std::string data(res->data, res->length);

        //std::cout << "@c:get #3 " << std::endl;
//        std::cout << "@c:get #3 [" << vv << "]" << std::endl;
	Handle<Value> oo = String::NewFromUtf8(isolate, data.c_str());
        args.GetReturnValue().Set(oo);
    }
}


void GetIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 2)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "Bad parameters"));
        return;
    }

    v8::String::Utf8Value str(args[ 0 ]);
    const char            *ticket = ToCString(str);

    v8::String::Utf8Value str1(args[ 1 ]);
    const char            *cstr = ToCString(str1);

    _Buff                 *doc_as_cbor = read_individual(ticket, str.length(), cstr, str1.length());

    if (doc_as_cbor != NULL)
    {
        std::string data(doc_as_cbor->data, doc_as_cbor->length);

        Individual  individual;
        //std::cout << "@c:get #1" << std::endl;
        cbor2individual(&individual, data);

        //std::cout << "@c:get #2" << std::endl;
        Handle<Value> oo = individual2jsobject(&individual, isolate);

        //Handle<String>	sss = toJson(oo);
        //    v8::String::Utf8Value s1(sss);
        //    std::string           vv = std::string(*s1);


        //std::cout << "@c:get #3 " << std::endl;
//        std::cout << "@c:get #3 [" << vv << "]" << std::endl;
        args.GetReturnValue().Set(oo);
    }
}

void PutIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int res = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        Individual individual;
        jsobject2individual(args[ 1 ], &individual, NULL, "");

        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char> buff;        
        individual2cbor(&individual, buff);
        char *ptr = buff.data ();
        res = put_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

void AddToIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int res = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        Individual individual;
        jsobject2individual(args[ 1 ], &individual, NULL, "");

        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char> buff;        
        individual2cbor(&individual, buff);
        char *ptr = buff.data ();
        res = add_to_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

void SetInIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int res = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        Individual individual;
        jsobject2individual(args[ 1 ], &individual, NULL, "");

        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char> buff;        
        individual2cbor(&individual, buff);
        char *ptr = buff.data ();
        res = set_in_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}

void RemoveFromIndividual(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    int res = 500;
    Isolate *isolate = args.GetIsolate();

    if (args.Length() != 3)
    {
        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "PutIndividual::Bad count parameters"));

        return;
    }

    if (args[ 1 ]->IsObject())
    {
        Individual individual;
        jsobject2individual(args[ 1 ], &individual, NULL, "");

        v8::String::Utf8Value str_ticket(args[ 0 ]);
        const char            *ticket = ToCString(str_ticket);

        v8::String::Utf8Value str_event_id(args[ 2 ]);
        const char            *event_id = ToCString(str_event_id);

        std::vector<char> buff;        
        individual2cbor(&individual, buff);
        char *ptr = buff.data ();
        res = remove_from_individual(ticket, str_ticket.length(), ptr, buff.size(), event_id, str_event_id.length());

        buff.clear();
    }

    args.GetReturnValue().Set(res);
}


// The callback that is invoked by v8 whenever the JavaScript 'print'
// function is called.  Prints its arguments on stdout separated by
// spaces and ending with a newline.
void Print(const v8::FunctionCallbackInfo<v8::Value>& args)
{
    bool first = true;

    for (int i = 0; i < args.Length(); i++)
    {
        v8::HandleScope handle_scope(args.GetIsolate());
        if (first)
        {
            first = false;
        }
        else
        {
            printf(" ");
        }
        v8::String::Utf8Value str(args[ i ]);
        const char            *cstr = ToCString(str);
        printf("%s", cstr);
    }
    printf("\n");
    fflush(stdout);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////

WrappedContext::WrappedContext()
{
    isolate_ = v8::Isolate::New();

    v8::Locker         locker(isolate_);
    v8::Isolate::Scope isolateScope(isolate_);
    HandleScope        handle_scope(isolate_);

    // Create a template for the global object.
    v8::Handle<v8::ObjectTemplate> global = v8::ObjectTemplate::New(isolate_);
    // Bind the global 'print' function to the C++ Print callback.
    global->Set(v8::String::NewFromUtf8(isolate_, "print"), v8::FunctionTemplate::New(isolate_, Print));

    global->Set(v8::String::NewFromUtf8(isolate_, "get_env_str_var"), v8::FunctionTemplate::New(isolate_, GetEnvStrVariable));

    global->Set(v8::String::NewFromUtf8(isolate_, "get_individual"), v8::FunctionTemplate::New(isolate_, GetIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "put_individual"), v8::FunctionTemplate::New(isolate_, PutIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "add_to_individual"), v8::FunctionTemplate::New(isolate_, AddToIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "set_in_individual"), v8::FunctionTemplate::New(isolate_, SetInIndividual));
    global->Set(v8::String::NewFromUtf8(isolate_, "remove_from_individual"), v8::FunctionTemplate::New(isolate_, RemoveFromIndividual));

    v8::Handle<v8::Context> context = v8::Context::New(isolate_, NULL, global);
    context_.Reset(isolate_, context);
}


WrappedContext::~WrappedContext()
{
//  context_.Dispose();
}


WrappedScript::~WrappedScript()
{
//  script_.Dispose();
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

WrappedContext *new_WrappedContext()
{
    WrappedContext *t = new WrappedContext();

    return t;
}

WrappedScript *new_WrappedScript(WrappedContext *_context, char *src)
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

void run_WrappedScript(WrappedContext *_context, WrappedScript *ws, _Buff *_res, _Buff *_out)
{
    Isolate                *isolate = _context->isolate_;

    v8::Locker             locker(isolate);
    v8::Isolate::Scope     isolateScope(isolate);

    HandleScope            scope(isolate);

    v8::Local<v8::Context> context = v8::Local<v8::Context>::New(isolate, _context->context_);

    Context::Scope         context_scope(context);

    v8::Local<v8::Script>  script = v8::Local<v8::Script>::New(isolate, ws->script_);

    Handle<Value>          result = script->Run();

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

void InitializeICU()
{
    v8::V8::InitializeICU(NULL);
}
