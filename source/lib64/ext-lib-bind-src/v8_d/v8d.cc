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
#include <stdlib.h>

#include <algorithm>
#include "cbor8json.h"
#include "msgpack8json.h"

using namespace std;
using namespace v8;

namespace
{
void FatalErrorCallback_r(const char *location, const char *message)
{
    std::cerr << "Fatal error in V8: " << location << " " << message;
}
}

/////// veda IO section //////////////////////////////////////////////////////////////////////////////////////////////////

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
    int        top           = 100000;
    int        limit         = 100000;

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

            if (args.Length() > 4)
            {
                top = args[ 4 ]->ToObject()->Uint32Value();

                if (args.Length() > 5)
                {
                    limit = args[ 5 ]->ToObject()->Uint32Value();
                }
            }
        }
    }

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

        Handle<Value> oo;
	if (data[0] == 0xFF)
	{
	    if (data.size () < 2)
	    {
	        isolate->ThrowException(v8::String::NewFromUtf8(isolate, "invalid msgpack, size < 2"));
		return;
	    }
    	    oo = msgpack2jsobject(isolate, data.substr (1, data.size ()));
	}
	else
    	    oo = cbor2jsobject(isolate, data);
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
