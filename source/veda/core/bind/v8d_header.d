/**
 * обвязка к v8d
 */
module veda.core.bind.v8d_header;

import std.stdio, std.conv;
import veda.type, veda.onto.individual, veda.onto.resource, veda.onto.lang, veda.core.common.context, veda.core.common.define,
       veda.util.cbor8individual, veda.core.util.utils;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "V8D");
    return _log;
}

// //////////////////////////  call D from C //////////////////////////////////////////

string[ string ] g_prop;
Context    g_context;

_Buff      g_super_classes;
_Buff      g_parent_script_id;
_Buff      g_parent_document_id;
_Buff      g_prev_state;
_Buff      g_execute_script;
_Buff      g_document;
_Buff      g_user;
_Buff      g_ticket;

_Buff      tmp_individual;

_Buff      g_script_result;
_Buff      g_script_out;

ResultCode g_last_result;

struct TransactionItem
{
    INDV_OP    cmd;
    string     indv_serl;
    string     ticket_id;
    string     event_id;
    ResultCode rc;

    Individual indv;

    this(INDV_OP _cmd, string _indv_serl, string _ticket_id, string _event_id)
    {
        cmd       = _cmd;
        indv_serl = _indv_serl;
        ticket_id = _ticket_id;
        event_id  = _event_id;

        int code = cbor2individual(&indv, indv_serl);
        if (code < 0)
        {
            rc = ResultCode.Unprocessable_Entity;
            log.trace("ERR:v8d:transaction:cbor2individual [%s]", indv_serl);
        }
        else
            rc = ResultCode.OK;

        indv.setStatus(rc);

        if (rc == ResultCode.OK && (cmd == INDV_OP.ADD_IN || cmd == INDV_OP.SET_IN || cmd == INDV_OP.REMOVE_FROM))
        {
            Individual      prev_indv;

            TransactionItem *ti = transaction_buff.get(indv.uri, null);
            if (ti !is null && ti.indv_serl.length > 0)
            {
                prev_indv = ti.indv;
            }
            else
            {
                Ticket *ticket = g_context.get_ticket(ticket_id);
                prev_indv = g_context.get_individual(ticket, indv.uri);
            }

            if (prev_indv.getStatus() == ResultCode.Connect_Error || prev_indv.getStatus() == ResultCode.Too_Many_Requests)
                rc = prev_indv.getStatus();

            if (prev_indv.getStatus() == ResultCode.OK)
                indv = *indv_apply_cmd(cmd, &prev_indv, &indv);
            else
                log.trace("g_context.get_individual[%s] fail=%s", indv.uri, prev_indv.getStatus());
        }
    }
}

TransactionItem *[ string ] transaction_buff;
TransactionItem *[] transaction_queue;

public bool commit()
{
    //if (transaction_buff.values.length > 0)
    //	writeln ("@ script: commit #1");

    foreach (item; transaction_queue)
    {
        if (item.indv == Individual.init)
            continue;

        if (item.rc != ResultCode.OK)
            return false;

        Ticket *ticket = g_context.get_ticket(item.ticket_id);

        //log.trace ("transaction: cmd=%s, indv=%s ", item.cmd, item.indv);

        ResultCode rc;
        if (item.cmd == INDV_OP.PUT)
        {
            rc = g_context.put_individual(ticket, item.indv.uri, item.indv, true, item.event_id, ignore_freeze).result;
        }
        else if (item.cmd == INDV_OP.ADD_IN)
        {
            rc = g_context.add_to_individual(ticket, item.indv.uri, item.indv, true, item.event_id, ignore_freeze).result;
        }
        else if (item.cmd == INDV_OP.SET_IN)
        {
            rc = g_context.set_in_individual(ticket, item.indv.uri, item.indv, true, item.event_id, ignore_freeze).result;
        }
        else if (item.cmd == INDV_OP.REMOVE_FROM)
        {
            rc = g_context.remove_from_individual(ticket, item.indv.uri, item.indv, true, item.event_id, ignore_freeze).result;
        }
        else if (item.cmd == INDV_OP.REMOVE)
        {
            rc = g_context.remove_individual(ticket, item.indv_serl, true, item.event_id, ignore_freeze).result;
        }

        if (rc != ResultCode.OK)
        {
            log.trace("FAIL COMMIT");
            return false;
        }
        //else
        //log.trace ("SUCCESS COMMIT");
    }



    //if (transaction_buff.values.length > 0)
    //	writeln ("@ script: commit #e");

    transaction_buff  = transaction_buff.init;
    transaction_queue = transaction_queue.init;

    //if (transaction_buff.values.length > 0)
    //	writeln ("@ script: commit #e1");

    return true;
}

extern (C++)
{
struct _Buff
{
    char *data;
    int  length;
    int  allocated_size;
}
}

extern (C++) char *get_global_prop(const char *prop_name, int prop_name_length)
{
    string pn  = cast(string)prop_name[ 0..prop_name_length ];
    string res = g_prop[ pn ];

    return cast(char *)res;
}

//чтение неправильное после операции add set
extern (C++) ResultCode put_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
                                       int _event_id_length)
{
    TransactionItem *ti = new TransactionItem(INDV_OP.PUT, cast(string)_cbor[ 0.._cbor_length ].dup, cast(string)_ticket[ 0.._ticket_length ].dup,
                                              cast(string)_event_id[ 0.._event_id_length ].dup);

    transaction_buff[ ti.indv.uri ] = ti;
    transaction_queue ~= ti;
    return ResultCode.OK;
}

extern (C++) ResultCode add_to_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
                                          int _event_id_length)
{
    TransactionItem *ti = new TransactionItem(INDV_OP.ADD_IN, cast(string)_cbor[ 0.._cbor_length ].dup, cast(string)_ticket[ 0.._ticket_length ].dup,
                                              cast(string)_event_id[ 0.._event_id_length ].dup);

    transaction_buff[ ti.indv.uri ] = ti;
    transaction_queue ~= ti;
    return ResultCode.OK;
}

extern (C++) ResultCode set_in_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
                                          int _event_id_length)
{
    TransactionItem *ti = new TransactionItem(INDV_OP.SET_IN, cast(string)_cbor[ 0.._cbor_length ].dup, cast(string)_ticket[ 0.._ticket_length ].dup,
                                              cast(string)_event_id[ 0.._event_id_length ].dup);

    transaction_buff[ ti.indv.uri ] = ti;
    transaction_queue ~= ti;
    return ResultCode.OK;
}

extern (C++) ResultCode remove_from_individual(const char *_ticket, int _ticket_length, const char *_cbor, int _cbor_length, const char *_event_id,
                                               int _event_id_length)
{
    TransactionItem *ti =
        new TransactionItem(INDV_OP.REMOVE_FROM, cast(string)_cbor[ 0.._cbor_length ].dup, cast(string)_ticket[ 0.._ticket_length ].dup,
                            cast(string)_event_id[ 0.._event_id_length ].dup);

    transaction_buff[ ti.indv.uri ] = ti;
    transaction_queue ~= ti;
    return ResultCode.OK;
}

extern (C++) ResultCode remove_individual(const char *_ticket, int _ticket_length, const char *_uri, int _uri_length, const char *_event_id,
                                          int _event_id_length)
{
    TransactionItem *ti = new TransactionItem(INDV_OP.REMOVE, cast(string)_uri[ 0.._uri_length ].dup, cast(string)_ticket[ 0.._ticket_length ].dup,
                                              cast(string)_event_id[ 0.._event_id_length ].dup);

    transaction_buff[ ti.indv.uri ] = ti;
    transaction_queue ~= ti;
    return ResultCode.OK;
}

////

extern (C++)_Buff * get_env_str_var(const char *_var_name, int _var_name_length)
{
    try
    {
        string var_name = cast(string)_var_name[ 0.._var_name_length ];

        if (var_name == "$parent_script_id")
        {
            return &g_parent_script_id;
        }
        else if (var_name == "$parent_document_id")
        {
            return &g_parent_document_id;
        }
        else if (var_name == "$user")
        {
            return &g_user;
        }
        else if (var_name == "$ticket")
        {
            return &g_ticket;
        }
        else if (var_name == "$super_classes")
        {
            return &g_super_classes;
        }

        return null;
    }
    finally
    {
        //writeln ("@p:v8d end read_individual");
    }
}

extern (C++)_Buff * read_individual(const char *_ticket, int _ticket_length, const char *_uri, int _uri_length)
{
    try
    {
        string          uri = cast(string)_uri[ 0.._uri_length ];

        TransactionItem *ti = transaction_buff.get(uri, null);
        if (ti !is null && ti.indv_serl.length > 0)
        {
            tmp_individual.data   = cast(char *)ti.indv_serl;
            tmp_individual.length = cast(int)ti.indv_serl.length;
            return &tmp_individual;
        }

        string ticket = cast(string)_ticket[ 0.._ticket_length ];

        //writeln ("@p:v8d read_individual, uri=[", uri, "],  ticket=[", ticket, "]");

        if (uri == "$document")
        {
            return &g_document;
        }
        else if (uri == "$prev_state")
        {
            if (g_prev_state.length > 0)
                return &g_prev_state;
            else
                return null;
        }
        else if (uri == "$execute_script")
        {
            if (g_execute_script.length > 0)
                return &g_execute_script;
            else
                return null;
        }
        else
        {
            if (g_context !is null)
            {
                string icb = g_context.get_individual_from_storage(uri);
                if (icb !is null)
                {
                    tmp_individual.data   = cast(char *)icb;
                    tmp_individual.length = cast(int)icb.length;
                    return &tmp_individual;
                }
                else
                    return null;
            }
            return null;
        }
    }
    finally
    {
        //writeln ("@p:v8d end read_individual");
    }
}

void dump(char *data, int count)
{
    string res;

    for (int i = 0; i < count; i++)
        res ~= "[" ~ text(cast(uint)data[ i ]) ~ "]";

    writeln("@d dump cbor=", res);
}

// //////////////////////////  call C from D //////////////////////////////////////////

extern (C++)
{
interface WrappedContext
{
}

interface WrappedScript
{
}

bool InitializeICU();
WrappedContext new_WrappedContext();
WrappedScript new_WrappedScript(WrappedContext _context, char *src);
void run_WrappedScript(WrappedContext _context, WrappedScript ws, _Buff *_res = null, _Buff *_out = null);
}

//alias new_WrappedContext new_ScriptVM;
//alias WrappedContext     ScriptVM;
//alias WrappedScript      Script;
//alias run_WrappedScript  run;
//alias new_WrappedScript  compile;

bool ignore_freeze;

class JsVM : ScriptVM
{
    WrappedContext js_vm;

    this()
    {
        js_vm = new_WrappedContext();
    }

    Script compile(string code)
    {
        Js res = new Js();

        res.vm     = this;
        res.script = new_WrappedScript(js_vm, cast(char *)(code ~ "\0"));
        return res;
    }
}

class Js : Script
{
    WrappedScript script;
    JsVM          vm;

    void run()
    {
        run_WrappedScript(vm.js_vm, script);
    }
}