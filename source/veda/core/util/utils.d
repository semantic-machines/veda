/**
 * utils
 */

module veda.core.util.utils;

private
{
    import core.stdc.stdio, core.stdc.string, core.sys.posix.time;
    import std.file, std.datetime, std.json, std.format, std.stdio, std.conv, std.string, std.concurrency;
    import std.ascii, std.csv, std.typecons, std.outbuffer;
    import veda.onto.individual, veda.onto.resource, veda.core.common.define, veda.util.container, veda.core.common.know_predicates,
           veda.core.common.context;
    import veda.common.type;
}

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "UTIL");
    return _log;
}
// ////// ////// ///////////////////////////////////////////
public Individual *indv_apply_cmd(INDV_OP cmd, Individual *prev_indv, Individual *indv)
{
    if (prev_indv !is null)
    {
        if (prev_indv.resources.get(rdf__type, Resources.init).length == 0)
        {
            log.trace("WARN! stores individual does not contain any type: arg:[%s] prev_indv:[%s]", text(*indv), text(*prev_indv));
        }

        foreach (predicate; indv.resources.keys)
        {
            if (cmd == INDV_OP.ADD_IN)
            {
                // add value to set or ignore if exists
                prev_indv.addUniqueResources(predicate, indv.getResources(predicate));
            }
            else if (cmd == INDV_OP.SET_IN)
            {
                // set value to predicate
                prev_indv.setResources(predicate, indv.getResources(predicate));
            }
            else if (cmd == INDV_OP.REMOVE_FROM)
            {
                // remove predicate or value in set
                prev_indv.removeResources(predicate, indv.getResources(predicate));
            }
        }
    }
    return prev_indv;
}


private Tid[ P_MODULE ] name_2_tids;

public Tid getTid(P_MODULE tid_id)
{
    if (name_2_tids.values.length == 0)
    {
        foreach (id; P_MODULE.min .. P_MODULE.max)
        {
            name_2_tids[ id ] = locate(text(id));
        }
    }

    Tid res = name_2_tids.get(tid_id, Tid.init);

    if (res == Tid.init)
    {
        // tid not found, attempt restore
        Tid tmp_tid = locate(text(tid_id));

        if (tmp_tid == Tid.init)
        {
//                writeln("!!! NOT FOUND TID=", text(tid_id), "\n", name_2_tids, ", locate=1 ", );
            //throw new Exception("!!! NOT FOUND TID=" ~ text(tid_id));
            log.trace("!!! NOT FOUND TID=%s", text(tid_id));
            return tmp_tid;
        }
        else
        {
            name_2_tids[ tid_id ] = tmp_tid;
            return tmp_tid;
        }
    }
    return res;
}



bool wait_starting_module(P_MODULE tid_idx, Tid tid)
{
    bool res;

    if (tid == Tid.init)
        throw new Exception("wait_starting_thread: Tid=" ~ text(tid_idx) ~ " not found", __FILE__, __LINE__);

    log.trace("START THREAD... : %s", text(tid_idx));
    send(tid, thisTid);
    receive((bool isReady)
            {
                res = isReady;
                //if (trace_msg[ 50 ] == 1)
                log.trace("START THREAD IS SUCCESS: %s", text(tid_idx));
                if (res == false)
                    log.trace("FAIL START THREAD: %s", text(tid_idx));
            });
    return res;
}

public string[ string ] getAsSimpleMapWithoutPrefix(Individual indv)
{
    string[ string ] res;

    foreach (key, val; indv.resources)
    {
        string   ss      = val[ 0 ].asString();
        string[] spl_key = key.split(':');
        if (spl_key.length > 1)
            key = spl_key[ 1 ];

        res[ key ] = ss;
    }

    return res;
}

/// serialize key2slot struct
public string serialize_key2slot(ref int[ string ] key2slot)
{
    OutBuffer outbuff = new OutBuffer();

    foreach (key, value; key2slot)
    {
        outbuff.write('"');
        outbuff.write(key);
        outbuff.write('"');
        outbuff.write(',');
        outbuff.write(text(value));
        outbuff.write('\n');
    }
    return outbuff.toString();
}

/// parse key2slot struct
public int[ string ] deserialize_key2slot(string data)
{
//	writeln ("@&1");
    int[ string ] key2slot;

    int idx = 0;
    foreach (record; csvReader!(Tuple!(string, int))(data))
    {
//	writeln ("@&2 record=[", record, "]");
        key2slot[ record[ 0 ] ] = record[ 1 ];
        idx++;
    }
//	writeln ("@&3");

    return key2slot;
}

string getNowAsString()
{
    SysTime sysTime = Clock.currTime();

    return sysTime.toISOExtString();
}

string timeToString(long tm)
{
    SysTime sysTime = SysTime(tm);

    return sysTime.toISOExtString();
}

string timeToString(SysTime sysTime)
{
    return sysTime.toISOExtString();
}

long stringToTime(string str)
{
    try
    {
        if (str.length == 28)
        {
            str = str[ 0..23 ];
        }

        SysTime st = SysTime.fromISOExtString(str);
        return st.stdTime;
    }
    catch (Exception ex)
    {
        return 0;
    }
}

public JSONValue read_props(string file_name)
{
    JSONValue res;

    if (std.file.exists(file_name))
    {
        char[] buff = cast(char[])std.file.read(file_name);

        res = parseJSON(buff);
    }
    else
    {
        JSONValue transport = JSONValue([ "point" : JSONValue("tcp://*:5559") ]);
        transport.object[ "transport" ] = JSONValue("zmq");

        JSONValue transport1 = JSONValue([ "transport" : JSONValue("file_reader") ]);

        JSONValue listeners = JSONValue([ transport, transport1 ]);
        res = JSONValue([ "listeners" : listeners ]);

        string buff = toJSON(&res);

        std.file.write(file_name, buff);
    }

    return res;
}
/*
   string fromStringz(char *s)
   {
    return cast(string)(s ? s[ 0 .. strlen(s) ] : null);
   }
 */
string fromStringz(char *s, int len)
{
    return cast(string)(s ? s[ 0 .. len ] : null);
}

// !!! stupid, but quickly
void formattedWrite(Writer, Char, A) (Writer w, in Char[] fmt, A[] args)
{
    if (args.length == 1)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ]);
        return;
    }
    else if (args.length == 2)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ]);
        return;
    }
    else if (args.length == 3)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ]);
        return;
    }
    else if (args.length == 4)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ]);
        return;
    }
    else if (args.length == 5)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ]);
        return;
    }
    else if (args.length == 6)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ]);
        return;
    }
    else if (args.length == 7)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ]);
        return;
    }
    else if (args.length == 8)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ]);
        return;
    }
    else if (args.length == 9)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ],
                                  args[ 8 ]);
        return;
    }
    else if (args.length == 10)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ]);
        return;
    }
    else if (args.length == 11)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ]);
        return;
    }
    else if (args.length == 12)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ]);
        return;
    }
    else if (args.length == 13)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ]);
        return;
    }
    else if (args.length == 14)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ], args[ 13 ]);
        return;
    }
    else if (args.length == 15)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ], args[ 13 ], args[ 14 ]);
        return;
    }
    else if (args.length == 16)
    {
        std.format.formattedWrite(w, fmt, args[ 0 ], args[ 1 ], args[ 2 ], args[ 3 ], args[ 4 ], args[ 5 ], args[ 6 ], args[ 7 ], args[ 8 ],
                                  args[ 9 ], args[ 10 ], args[ 11 ], args[ 12 ], args[ 13 ], args[ 14 ], args[ 15 ]);
        return;
    }

    throw new Exception("util.formattedWrite (), count args > 16");
}

private static string[ dchar ] translit_table;

static this()
{
    translit_table =
    [
        '№':"N", ',':"_", '-':"_", ' ':"_", 'А':"A", 'Б':"B", 'В':"V", 'Г':"G", 'Д':"D", 'Е':"E", 'Ё':"E",
        'Ж':"ZH", 'З':"Z", 'И':"I", 'Й':"I", 'К':"K", 'Л':"L", 'М':"M", 'Н':"N", 'О':"O", 'П':"P", 'Р':"R",
        'С':"S", 'Т':"T", 'У':"U", 'Ф':"F", 'Х':"H", 'Ц':"C", 'Ч':"CH", 'Ш':"SH", 'Щ':"SH", 'Ъ':"'", 'Ы':"Y",
        'Ь':"'", 'Э':"E", 'Ю':"U", 'Я':"YA", 'а':"a", 'б':"b", 'в':"v", 'г':"g", 'д':"d", 'е':"e", 'ё':"e",
        'ж':"zh", 'з':"z", 'и':"i", 'й':"i", 'к':"k", 'л':"l", 'м':"m", 'н':"n", 'о':"o", 'п':"p", 'р':"r",
        'с':"s", 'т':"t", 'у':"u", 'ф':"f", 'х':"h", 'ц':"c", 'ч':"ch", 'ш':"sh", 'щ':"sh", 'ъ':"_", 'ы':"y",
        'ь':"_", 'э':"e", 'ю':"u", 'я':"ya"
    ];
}

/**
 * Переводит русский текст в транслит. В результирующей строке каждая
 * русская буква будет заменена на соответствующую английскую. Не русские
 * символы останутся прежними.
 *
 * Parameters: text
 *            исходный текст с русскими символами
 * Returns: результат
 */
public static string toTranslit(string text)
{
    return translate(text, translit_table);
}

public JSONValue[] get_array(JSONValue jv, string field_name)
{
    if (field_name in jv.object)
    {
        return jv.object[ field_name ].array;
    }
    return null;
}

public string get_str(JSONValue jv, string field_name)
{
    if (field_name in jv.object)
    {
        return jv.object[ field_name ].str;
    }
    return null;
}

public long get_int(JSONValue jv, string field_name)
{
    if (field_name in jv.object)
    {
        return jv.object[ field_name ].integer;
    }
    return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 * возвращает локальное время
 *
 * Returns: структура tm
 */
public tm *get_local_time()
{
    time_t rawtime;
    tm     *timeinfo;

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    return timeinfo;
}

public string get_year(tm *timeinfo)
{
    return text(timeinfo.tm_year + 1900);
}

public string get_month(tm *timeinfo)
{
    if (timeinfo.tm_mon < 9)
        return "0" ~ text(timeinfo.tm_mon + 1);
    else
        return text(timeinfo.tm_mon + 1);
}

public string get_day(tm *timeinfo)
{
    if (timeinfo.tm_mday < 10)
        return "0" ~ text(timeinfo.tm_mday);
    else
        return text(timeinfo.tm_mday);
}

public int cmp_date_with_tm(string date, tm *timeinfo)
{
    string today_y = get_year(timeinfo);
    string today_m = get_month(timeinfo);
    string today_d = get_day(timeinfo);

    for (int i = 0; i < 4; i++)
    {
        if (date[ i + 6 ] > today_y[ i ])
        {
            return 1;
        }
        else if (date[ i + 6 ] < today_y[ i ])
        {
            return -1;
        }
    }

    for (int i = 0; i < 2; i++)
    {
        if (date[ i + 3 ] > today_m[ i ])
        {
            return 1;
        }
        else if (date[ i + 3 ] < today_m[ i ])
        {
            return -1;
        }
    }

    for (int i = 0; i < 2; i++)
    {
        if (date[ i ] > today_d[ i ])
        {
            return 1;
        }
        else if (date[ i ] < today_d[ i ])
        {
            return -1;
        }
    }

    return 0;
}

public bool is_today_in_interval(string from, string to)
{
    tm *timeinfo = get_local_time();

    if (from !is null && from.length == 10 && cmp_date_with_tm(from, timeinfo) > 0)
        return false;

    if (to !is null && to.length == 10 && cmp_date_with_tm(to, timeinfo) < 0)
        return false;

    return true;
}

string to_lower_and_replace_delimeters(string in_text)
{
    if (in_text is null || in_text.length == 0)
        return in_text;

    char[] out_text = new char[ in_text.length ];

    for (int i = 0; i < in_text.length; i++)
    {
        char cc = in_text[ i ];
        if (cc == ':' || /*cc == ' ' ||*/ cc == '-')
            out_text[ i ] = '_';
        else
            out_text[ i ] = std.ascii.toLower(cc);
    }

    return cast(immutable)out_text;
}

////////////

string uxxxx2utf8(string so)
{
    string new_s;
    bool   susecs = false;

    for (int i = 0; i < so.length; i++)
    {
        char c = so[ i ];
        if (c == '\\')
        {
            if (so[ i + 1 ] == 'u')
            {
                string qqq = so[ i + 2..i + 6 ];
                try
                {
                    int jjj = qqq.to!uint (16);
                    if (susecs == false)
                        new_s ~= so[ 0..i ];
                    i += 5;
                    new_s ~= cast(dchar)jjj;
                    susecs = true;
                }
                catch (Exception ex)
                {
                }
            }
            else if (so[ i + 1 ] == 'n')
            {
                if (susecs == false)
                    new_s ~= so[ 0..i ];
                i += 1;
                new_s ~= '\n';
                susecs = true;
            }
            else if (so[ i + 1 ] == 't')
            {
                if (susecs == false)
                    new_s ~= so[ 0..i ];
                i += 1;
                new_s ~= '\t';
                susecs = true;
            }
            else if (so[ i + 1 ] == '"')
            {
                if (susecs == false)
                    new_s ~= so[ 0..i ];
                i += 1;
                new_s ~= '"';
                susecs = true;
            }
            else
            {
                new_s ~= c;
            }
        }
        else
        {
            if (susecs == true)
                new_s ~= c;
        }
    }
    if (susecs == true)
        return new_s;
    else
        return so;
}



//////////////////////////////////////////////// MODULES INTERACTION
struct ModulezInfo
{
    string name;
    long   op_id;
    long   committed_op_id = -1;
    bool   is_Ok;
}

private string[ P_MODULE ] fn_info_r__2__pmodule;
private File *[ P_MODULE ] ff_info_r__2__pmodule;
private ubyte[] buff;

public ModulezInfo get_info(P_MODULE _module)
{
//log.trace ("get_info (%s)", text(_module));

    ModulezInfo res;

    res.is_Ok = false;
    try
    {
        string fn_info_r = fn_info_r__2__pmodule.get(_module, null);
		File *ff_info_r;
		
        if (fn_info_r is null)
        {
            fn_info_r = module_info_path ~ "/" ~ text(_module) ~ "_info";
            ff_info_r = new File(fn_info_r, "r");
            if (ff_info_r.isOpen())
            {
	            fn_info_r__2__pmodule[ _module ] = fn_info_r;
	            ff_info_r__2__pmodule[ _module ] = ff_info_r;
            }
            else
	            return res;    
        }
        else
        {
	        ff_info_r = ff_info_r__2__pmodule.get(_module, null);
        }   
        
        if (ff_info_r !is null)
        {
			ff_info_r.seek(0);
			
			if (buff is null) buff  = new ubyte[ 4096];
			        	
			ubyte[] newbuff = ff_info_r.rawRead(buff);
			if (newbuff.length > 3)
			{
            string str = cast(string)newbuff[0..$];
            if (str !is null)
            {            	
                string[] ch = str[ 0..$ - 1 ].split(';');
                if (ch.length != 3)
                {
                    return res;
                }
                res.name            = ch[ 0 ];
                res.op_id           = to!long (ch[ 1 ]);
                res.committed_op_id = to!long (ch[ 2 ]);
                res.is_Ok = true;
            }
			}
        }
    }
    catch (Throwable tr)
    {
        log.trace("vmodule[%s]:get_info fail, err=%s", _module, tr.msg);
    }

                    	//writefln ("info[%s], res(%s): name=%s, op_id=%d, committed_op_id=%d", text (_module), text (res.is_Ok), res.name, res.op_id, res.committed_op_id);
    return res;
}    
  
  
bool put_module_info (File *ff_module_info_w, string process_name, long op_id, long committed_op_id)
{
        try
        {
            ff_module_info_w.seek(0);
            ff_module_info_w.writefln("%s;%d;%d", process_name, op_id, committed_op_id);
            ff_module_info_w.flush();
            log.trace("module:put_info [%s;%d;%d]", process_name, op_id, committed_op_id);
            return true;
        }
        catch (Throwable tr)
        {
            log.trace("module:put_info [%s;%d;%d] %s", process_name, op_id, committed_op_id, tr.msg);
            return false;
        }
}  
    
