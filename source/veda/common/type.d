/**
 * Общие определения

   Copyright: © 2014-2018 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev
 */
module veda.common.type;

import std.math, std.stdio, std.conv, std.string;


/**
 * Коды результата выполнения
 */
public enum ResultCode
{
    /// 0
    zero                    = 0,

    /// 200
    Ok                      = 200,

    /// 201
    Created                 = 201,

    /// 204
    NoContent               = 204,

    /// 400
    BadRequest              = 400,

    /// 403
    Forbidden               = 403,

    /// 404
    NotFound                = 404,

    /// 422
    UnprocessableEntity     = 422,

    /// 429
    TooManyRequests         = 429,

    /// 464
    SecretExpired           = 464,

    /// 465
    EmptyPassword           = 465,

    /// 466
    NewPasswordIsEqualToOld = 466,

    /// 467
    InvalidPassword         = 467,

    /// 468
    InvalidSecret           = 468,

    /// 469
    PasswordExpired         = 469,

    /// 470
    TicketNotFound          = 470,

    /// 471
    TicketExpired           = 471,

    /// 472
    NotAuthorized           = 472,

    /// 473
    AuthenticationFailed    = 473,

    /// 474
    NotReady                = 474,

    /// 475
    FailOpenTransaction     = 475,

    /// 476
    FailCommit              = 476,

    /// 477
    FailStore               = 477,

    /// 500
    InternalServerError     = 500,

    /// 501
    NotImplemented          = 501,

    /// 503
    ServiceUnavailable      = 503,

    InvalidIdentifier       = 904,

    /// 999
    DatabaseModifiedError   = 999,

    /// 1021
    DiskFull                = 1021,

    /// 1022
    DuplicateKey            = 1022,

    /// 1118
    SizeTooLarge            = 1118,

    /// 4000
    ConnectError            = 4000
}

/// Uri
alias string Uri;

/// Перечисление - Типы данных
public enum DataType : ubyte
{
    /// URI
    Uri      = 1,

    /// Строка
    String   = 2,

    /// Целочисленное число
    Integer  = 4,

    /// Время
    Datetime = 8,

    /// Десятичное число
    Decimal  = 32,

    /// Boolean
    Boolean  = 64
}

string nullz         = "00000000000000000000000000000000";

/// Десятичное число
struct decimal
{
    /// мантисса
    long mantissa;

    /// экспонента
    byte exponent;

    this(string m, string e)
    {
        mantissa = to!long (m);
        exponent = to!byte (e);
    }

    /// конструктор
    this(long m, byte e)
    {
        mantissa = m;
        exponent = e;
    }

    /// конструктор
    this(string num)
    {
        if (num is null)
            return;

        string[] ff;

        if (num.indexOf(',') > 0)
            ff = split(num, ",");
        else
            ff = split(num, ".");

        if (ff.length == 2)
        {
            byte sfp = cast(byte)ff[ 1 ].length;

            mantissa = to!long (ff[ 0 ] ~ff[ 1 ]);
            exponent = cast(byte)(exponent - sfp);
        }
        else if (ff.length == 1)
        {
            mantissa = to!long (num);
            exponent = 0;
        }
    }

    /// конструктор
    this(double num)
    {
        byte sign = 1;

        if (num < 0)
        {
            num  = -num;
            sign = -1;
        }

        byte   count;
        double x = num;
        while (true)
        {
            if (x - cast(long)(x) <= 0)
                break;

            x *= 10;
            ++count;
        }
        mantissa = cast(long)(num * pow(10L, count)) * sign;
        exponent = cast(byte)(exponent - count);
    }

    /// вернуть double
    double toDouble()
    {
        try
        {
            return mantissa * pow(10.0, exponent);
        }
        catch (Exception ex)
        {
            writeln("EX! ", ex.msg);
            return 0;
        }
    }

    string asString()
    {
        string str_res;
        string sign = "";
        string str_mantissa;

        if (mantissa < 0)
        {
            sign         = "-";
            str_mantissa = text(-mantissa);
        }
        else
            str_mantissa = text(mantissa);

        long lh = exponent * -1;

        lh = str_mantissa.length - lh;
        string slh;

        if (lh >= 0)
        {
            if (lh <= str_mantissa.length)
                slh = str_mantissa[ 0 .. lh ];
        }
        else
            slh = "";

        string slr;

        if (lh >= 0)
        {
            slr = str_mantissa[ lh..$ ];
        }
        else
        {
            slr = nullz[ 0.. (-lh) ] ~str_mantissa;
        }

        str_res = sign ~ slh ~ "." ~ slr;
        return str_res;
    }

    ///
    double toDouble_wjp()
    {
        string str_res;
        double res;
        bool   is_complete = false;

        if (exponent < 0)
        {
            string str_mantissa = text(mantissa);
            try
            {
                long lh = exponent * -1;
                lh = str_mantissa.length - lh;

                if (lh > 0 && lh > str_mantissa.length)
                {
                    res         = toDouble();
                    is_complete = true;
                }

                string slh;

                if (is_complete == false)
                {
                    if (lh >= 0)
                    {
                        if (lh > str_mantissa.length)
                        {
                            res         = toDouble();
                            is_complete = true;
                        }
                        else
                            slh = str_mantissa[ 0 .. lh ];
                    }
                    else
                        slh = "";

                    string slr;

                    if (lh >= 0)
                    {
                        slr = str_mantissa[ lh..$ ];
                    }
                    else
                        slr = nullz[ 0.. (-lh) ] ~str_mantissa;

                    str_res = slh ~ "." ~ slr;

                    res = to!double (str_res);
                }
            }
            catch (Exception ex)
            {
                res = toDouble();
            }
        }
        else
        {
            res = toDouble();
        }
        return res;
    }
}
