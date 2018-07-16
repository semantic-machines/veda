/**
 * CBOR общее
 */
module veda.onto.bj8individual.cbor;

private
{
    import std.outbuffer, std.stdio, std.typetuple, std.datetime, std.conv;
    import veda.util.container, veda.util.tools;
}

enum : byte
{
    TYPE  = 1,
    LINKS = 2,
    ALL   = 4
}

/** The CBOR-encoded boolean <code>false</code> value (encoded as "simple value": {@link #MT_SIMPLE}). */
ubyte FALSE = 0x14;
/** The CBOR-encoded boolean <code>true</code> value (encoded as "simple value": {@link #MT_SIMPLE}). */
ubyte TRUE = 0x15;
/** The CBOR-encoded <code>null</code> value (encoded as "simple value": {@link #MT_SIMPLE}). */
ubyte NULL = 0x16;
/** The CBOR-encoded "undefined" value (encoded as "simple value": {@link #MT_SIMPLE}). */
byte  UNDEFINED = 0x17;

/** Denotes a half-precision float (two-byte IEEE 754, see {@link #MT_FLOAT}). */
int HALF_PRECISION_FLOAT = 0x19;
/** Denotes a single-precision float (four-byte IEEE 754, see {@link #MT_FLOAT}). */
int SINGLE_PRECISION_FLOAT = 0x1a;
/** Denotes a double-precision float (eight-byte IEEE 754, see {@link #MT_FLOAT}). */
int DOUBLE_PRECISION_FLOAT = 0x1b;
/** The CBOR-encoded "break" stop code for unlimited arrays/maps. */
int BREAK = 0x1f;

/** Semantic tag value describing CBOR content. */
int TAG_CBOR_MARKER = 55799;

/// Основные типы данных CBOR
enum MajorType : ubyte
{
    /** Major type 0: unsigned integers. */
    UNSIGNED_INTEGER = 0 << 5,

    /** Major type 1: negative integers. */
    NEGATIVE_INTEGER = 1 << 5,

    /** Major type 2: byte string. */
    BYTE_STRING      = 2 << 5,

    /** Major type 3: text/UTF8 string. */
    TEXT_STRING      = 3 << 5,

    /** Major type 4: array of items. */
    ARRAY            = 4 << 5,

    /** Major type 5: map of pairs. */
    MAP              = 5 << 5,

    /** Major type 6: semantic tags. */
    TAG              = 6 << 5,

    /** Major type 7: floating point, simple data types. */
    FLOAT_SIMPLE     = 7 << 5,

    NONE             = 255
}

/// Теги CBOR
enum TAG : ubyte
{
    NONE                        = 255,

    TEXT_RU                     = 42,

    TEXT_EN                     = 43,
/** date/time values in the standard format (UTF8 string, RFC3339). */
    STANDARD_DATE_TIME          = 0,

/** date/time values as Epoch timestamp (numeric, RFC3339). */
    EPOCH_DATE_TIME             = 1,

/** positive big integer value (byte string). */
    POSITIVE_BIGINT             = 2,

/** negative big integer value (byte string). */
    NEGATIVE_BIGINT             = 3,

/** decimal fraction value (two-element array, base 10). */
    DECIMAL_FRACTION            = 4,

/** big decimal value (two-element array, base 2). */
    BIGDECIMAL                  = 5,

/** base64url encoding. */
    EXPECTED_BASE64_URL_ENCODED = 21,

/** base64 encoding. */
    EXPECTED_BASE64_ENCODED     = 22,

/** base16 encoding. */
    EXPECTED_BASE16_ENCODED     = 23,

/** encoded CBOR data item (byte string). */
    CBOR_ENCODED                = 24,

/** URL (UTF8 string). */
    URI                         = 32,

/** base64url encoded string (UTF8 string). */
    BASE64_URL_ENCODED          = 33,

/** base64 encoded string (UTF8 string). */
    BASE64_ENCODED              = 34,

/** regular expression string (UTF8 string, PCRE). */
    REGEXP                      = 35,

/** MIME message (UTF8 string, RFC2045). */
    MIME_MESSAGE                = 36
}

/// Элемент/заголовок
struct ElementHeader
{
    /// Тип
    MajorType type = MajorType.NONE;

    /// Число
    long      v_long;

    /// Тэг
    TAG       tag = TAG.NONE;
}


string toString(ElementHeader *el)
{
    return "type=" ~ text(el.type) ~ ", len=" ~ text(el.v_long) ~ ", tag=" ~ text(el.tag);
}

///////////////////////////////////////////////////////////////////////////

/// Записать в буффер тип и значение
public void write_type_value(MajorType type, ulong value, ref OutBuffer ou)
{
    ubyte element_header;

    if (value < 24)
    {
        ubyte ll = cast(ubyte)value;
        element_header = type | ll;
        ou.write(element_header);
    }
    else
    {
        if (value > uint.max)
        {
            element_header = type | 27;
            ou.write(element_header);
            ou.write(value);
        }
        else if (value > ushort.max)
        {
            element_header = type | 26;
            ou.write(element_header);
            ou.write(cast(uint)value);
        }
        else if (value > ubyte.max)
        {
            element_header = type | 25;
            ou.write(element_header);
            ou.write(cast(ushort)value);
//            writeln ("@p #write cast(ushort)value=", cast(ushort)value, ", value=", value);
        }
        else
        {
            element_header = type | 24;
            ou.write(element_header);
            ou.write(cast(ubyte)value);
        }
//        writeln ("@ element_header=", element_header);
//          writeln ("@ value=", value);
    }
}


/// Записать в буффер integer
public void write_integer(long vv, ref OutBuffer ou)
{
    if (vv >= 0)
        write_type_value(MajorType.UNSIGNED_INTEGER, vv, ou);
    else
        write_type_value(MajorType.NEGATIVE_INTEGER, -vv, ou);
}

/// Записать в буффер строку
public void write_string(string vv, ref OutBuffer ou)
{
    write_type_value(MajorType.TEXT_STRING, vv.length, ou);
    ou.write(vv);
}

/// Записать в буффер boolean
public void write_bool(bool vv, ref OutBuffer ou)
{
    if (vv == true)
        write_type_value(MajorType.FLOAT_SIMPLE, TRUE, ou);
    else
        write_type_value(MajorType.FLOAT_SIMPLE, FALSE, ou);
}

/// Читать из буффера тип и значение
public int read_type_value(ubyte[] src, ElementHeader *header)
{
    if (src.length == 0)
        return 0;

    ubyte     hh = src[ 0 ];

    MajorType type = cast(MajorType)(hh & 0xe0);

    long      ld    = hh & 0x1f;
    int       d_pos = 1;

    if (ld > 23)
    {
        d_pos += 1 << (ld - 24);

        if (ld == 24)
            ld = src[ 1 ];
        else if (ld == 25)
            ld = ushort_from_buff(src, 1);
        else if (ld == 26)
            ld = uint_from_buff(src, 1);
        else if (ld == 27)
            ld = ulong_from_buff(src, 1);
    }

    //if (type == MajorType.NEGATIVE_INTEGER || type == MajorType.UNSIGNED_INTEGER)
    //{
    //    writeln ("@p res_ld=", ld);
    //}

    if (type == MajorType.TAG)
    {
        ElementHeader main_type_header;
        int           len = read_type_value(src[ d_pos..$ ], &main_type_header);
        if (len <= 0)
        {
            //writeln("@%%%1");
            throw new Exception("no content in pos");
        }
        d_pos        += len;
        header.tag    = cast(TAG)ld;
        header.v_long = main_type_header.v_long;
        header.type   = main_type_header.type;
//      writeln ("HEADER:", header.toString());
    }
    else
    {
        if (type == MajorType.NEGATIVE_INTEGER)
        {
            ld = -ld;
            //writeln ("@p #type=", text(type), ", ld=", ld);
        }
        else if ((type == MajorType.ARRAY || type == MajorType.TEXT_STRING) && ld > src.length)
        {
            //writeln("Err! @d cbor.read_header, ld=", ld);
            throw new Exception("read_type_value: cbor.read_header, ld=" ~ text(ld) ~ ", src=[" ~ text(src) ~ "]");
            //ld = src.length;
        }

        header.v_long = ld;
        header.type   = type;
    }
//    writeln ("type=", type, ", length=", ld, ", d_pos=", d_pos, ", src.length=", src.length);

    return d_pos;
}

void hexdump(T) (string z, T[] s)
{
    char[] pad = z.dup;
    pad[] = ' ';

    writefln("%s, %s * %s", z, s.length, T.sizeof);

    writef("%s --> ", z);
    byte *b = cast(byte *)s.ptr;
    int  N  = cast(int)(s.length * T.sizeof);

    for (int i = 0; i < N; i++)
    {
        writef("%2.2x ", b[ i ]);

        if ((i & 7) == 7)
            writef(" ", pad);

        if ((i & 15) == 15)
        {
            writef("    ", pad);
            for (int j = i - 15; j <= i; j++)
                writef("%s", cast(char)b[ j ]);

            writef("\n %s    ", pad);
        }
    }
    writefln("\n");
}