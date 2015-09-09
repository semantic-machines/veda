/**
 * utils
 */

module util.utils;

private
{
    import core.stdc.stdio;
    import std.file, std.datetime, std.json, std.c.string, std.c.linux.linux, std.format, std.stdio, std.conv, std.string, std.concurrency;
    import std.ascii, std.csv, std.typecons, std.outbuffer;
    import veda.onto.individual, veda.onto.resource;
    import util.container;
    import veda.core.know_predicates, veda.core.context;
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

public class stack(T)
{
    T[] data;
    int pos;

    this()
    {
        data = new T[ 100 ];
        pos  = 0;
    }

    T back()
    {
        //		writeln("stack:back:pos=", pos, ", data=", data[pos]);
        return data[ pos ];
    }

    T popBack()
    {
        if (pos > 0)
        {
            //			writeln("stack:popBack:pos=", pos, ", data=", data[pos]);
            pos--;
            return data[ pos + 1 ];
        }
        return data[ pos ];
    }

    void pushBack(T val)
    {
        //		writeln("stack:pushBack:pos=", pos, ", val=", val);
        pos++;
        data[ pos ] = val;
    }

    bool empty()
    {
        return pos == 0;
    }
}

string _tmp_correct_link(string link)
{
    // TODO убрать корректировки ссылок в organization: временная коррекция ссылок
    char[] sscc = link.dup;
    if (sscc[ 7 ] == '_')
        sscc = sscc[ 8..$ ];
    else if (sscc[ 8 ] == '_')
        sscc = sscc[ 9..$ ];
    return cast(string)sscc;
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

string escaping_or_uuid2search(string in_text)
{
    OutBuffer outbuff = new OutBuffer();

    escaping_or_uuid2search(in_text, outbuff);
    return outbuff.toString;
}

void escaping_or_uuid2search(string in_text, ref OutBuffer outbuff)
{
    bool need_prepare = false;

    int  idx = 0;

    foreach (ch; in_text)
    {
        if (ch == '-')
        {
            need_prepare = true;
            break;
        }
        if (ch == '"' || ch == '\n' || ch == '\\' || ch == '\t' || (ch == ':' && idx < 5))
        {
            need_prepare = true;
//			break;
        }
        idx++;
    }

    if (need_prepare)
    {
        int len = cast(uint)in_text.length;

        for (int i = 0; i < len; i++)
        {
            if (i >= len)
                break;

            char ch = in_text[ i ];

            if ((ch == '"' || ch == '\\'))
            {
                outbuff.write('\\');
                outbuff.write(ch);
            }
            else if (ch == '\n')
            {
                outbuff.write("\\n");
            }
            else if (ch == '\t')
            {
                outbuff.write("\\t");
            }
            else
            {
                if (ch == '-' || ch == ':')
                    outbuff.write('_');
                else
                    outbuff.write(ch);
            }
        }
    }
    else
    {
        outbuff.write(in_text);
    }
}

//////////////////////////////////////////////////////////////////////////////
void print_2(ref Set!string *[ string ] res)
{
    writeln("***");
    foreach (key; res.keys)
    {
        writeln(key, ":");
        Set!string * ss = res[ key ];
        foreach (aa; ss.items)
        {
            writeln("	", aa);
        }
    }
}

// based on std.functional.memoize
/*
         Copyright Andrei Alexandrescu 2008 - 2009.
   Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
 */

enum cacheize_use
{
    common_use,
    execute_and_update
}

template cacheize(alias fun, uint maxSize = uint.max)
{
    ReturnType!fun cacheize(ParameterTypeTuple!fun args, cacheize_use use = cacheize_use.common_use)
    {
        static ReturnType!fun[ Tuple!(typeof(args)) ] memo;
        auto   t = tuple(args);

        if (use == cacheize_use.common_use)
        {
            auto p = t in memo;
            if (p)
                return *p;

            static if (maxSize != uint.max)
            {
                if (memo.length >= maxSize)
                    memo = null;
            }
        }
        auto r = fun(args);
        //writeln("Inserting result ", typeof(r).stringof, "(", r, ") for parameters ", t);
        memo[ t ] = r;
        return r;
    }
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
