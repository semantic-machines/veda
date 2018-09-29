/**
 * Общие определения

   Copyright: © 2014-2017 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev
 */
module veda.common.type;

import std.math, std.stdio, std.conv, std.string;

/**
 * Обьект - сессионный тикет
 */
public struct Ticket
{
    /// ID
    string     id;

    /// Uri пользователя
    string     user_uri;

    /// login пользователя
    string     user_login;

    /// Код результата, если тикет не валидный != ResultCode.Ok
    ResultCode result;

    /// Дата начала действия тикета
    long       start_time;

    /// Дата окончания действия тикета
    long       end_time;

    /// Конструктор
    this(Ticket tt)
    {
        id       = tt.id.dup;
        user_uri = tt.user_uri.dup;
        end_time = tt.end_time;
    }

    this(string _id, string _user_uri, long _end_time)
    {
        id       = _id;
        user_uri = _user_uri;
        end_time = _end_time;
    }
}

/// id подсистем
public enum SUBSYSTEM : ubyte
{
    NONE              = 0,
    STORAGE           = 1,
    ACL               = 2,
    FULL_TEXT_INDEXER = 4,
    FANOUT_EMAIL      = 8,
    SCRIPTS           = 16,
    FANOUT_SQL        = 32,
    USER_MODULES_TOOL = 64
}

/// id компонентов
public enum COMPONENT : ubyte
{
    /// сохранение индивидуалов
    subject_manager  = 1,

    /// Индексирование прав
    acl_preparer     = 2,

    /// Полнотекстовое индексирование
    fulltext_indexer = 4,

    /// Отправка email
    fanout_email     = 8,

    /// исполнение скриптов, normal priority
    scripts_main     = 16,

    /// Выдача и проверка тикетов
    ticket_manager   = 29,

    /// Выгрузка в sql, низкоприоритетное исполнение
    fanout_sql_lp    = 30,

    /// Выгрузка в sql, высокоприоритетное исполнение
    fanout_sql_np    = 32,

    /// исполнение скриптов, low priority
    scripts_lp       = 33,

    //// long time run scripts
    ltr_scripts      = 34,

    ///////////////////////////////////////

    /// Сбор статистики
    statistic_data_accumulator = 35,

    /// Сохранение накопленных в памяти данных
    commiter                   = 36,

    /// Вывод статистики
    print_statistic            = 37,

    n_channel                  = 38,

    /// Загрузка из файлов
    file_reader                = 40,

    input_queue                = 41,

    user_modules_tool          = 64
}


/// id процессов
public enum P_MODULE : ubyte
{
    ticket_manager             = COMPONENT.ticket_manager,
    subject_manager            = COMPONENT.subject_manager,
    acl_preparer               = COMPONENT.acl_preparer,
    statistic_data_accumulator = COMPONENT.statistic_data_accumulator,
    commiter                   = COMPONENT.commiter,
    print_statistic            = COMPONENT.print_statistic,
    file_reader                = COMPONENT.file_reader,
    n_channel                  = COMPONENT.n_channel,
}

/// id модулей обрабатывающих очередь
public enum MODULE : ubyte
{
    ticket_manager    = COMPONENT.ticket_manager,
    subject_manager   = COMPONENT.subject_manager,
    acl_preparer      = COMPONENT.acl_preparer,
    fulltext_indexer  = COMPONENT.fulltext_indexer,
    scripts_main      = COMPONENT.scripts_main,
    scripts_lp        = COMPONENT.scripts_lp,
    fanout_email      = COMPONENT.fanout_email,
    user_modules_tool = COMPONENT.user_modules_tool,
    ltr_scripts       = COMPONENT.ltr_scripts,
    fanout_sql_np     = COMPONENT.fanout_sql_np,
    fanout_sql_lp     = COMPONENT.fanout_sql_lp,
    input_queue       = COMPONENT.input_queue
}

/**
 * Коды результата выполнения
 */
public enum ResultCode
{
    /// 0
    zero                         = 0,

    /// 200
    OK                           = 200,

    /// 201
    Created                      = 201,

    /// 204
    No_Content                   = 204,

    /// 400
    Bad_Request                  = 400,

    /// 403
    Forbidden                    = 403,

    /// 404
    Not_Found                    = 404,

    /// 422
    Unprocessable_Entity         = 422,

    /// 429
    Too_Many_Requests            = 429,

    /// 464
    Secret_expired               = 464,

    /// 465
    Empty_password               = 465,

    /// 466
    New_password_is_equal_to_old = 466,

    /// 467
    Invalid_password             = 467,

    /// 468
    Invalid_secret               = 468,

    /// 469
    Password_expired             = 469,

    /// 470
    Ticket_not_found             = 470,

    /// 471
    Ticket_expired               = 471,

    /// 472
    Not_Authorized               = 472,

    /// 473
    Authentication_Failed        = 473,

    /// 474
    Not_Ready                    = 474,

    /// 475
    Fail_Open_Transaction        = 475,

    /// 476
    Fail_Commit                  = 476,

    /// 477
    Fail_Store                   = 477,

    /// 500
    Internal_Server_Error        = 500,

    /// 501
    Not_Implemented              = 501,

    /// 503
    Service_Unavailable          = 503,

    Invalid_Identifier           = 904,

    /// 999
    DatabaseModifiedError        = 999,

    /// 1021
    Disk_Full                    = 1021,

    /// 1022
    Duplicate_Key                = 1022,

    /// 1118
    Size_too_large               = 1118,

    /// 4000
    Connect_Error                = 4000
}

enum OptFreeze
{
    INGORE,
    NONE
}

enum OptAuthorize
{
    NO,
    YES
}

enum OptTrace
{
    TRACE,
    NONE
}

public struct OpResult
{
    ResultCode result;
    long       op_id;
}

/// Результат
public enum Result
{
    /// OK
    Ok,

    /// Ошибка
    Err,

    /// Ничего
    Nothing
}

/// Uri
alias string Uri;

/// Битовые поля для прав
public enum Access : ubyte
{
    /// Создание
    can_create  = 1,

    /// Чтение
    can_read    = 2,

    /// Изменеие
    can_update  = 4,

    /// Удаление
    can_delete  = 8,

    /// Запрет создания
    cant_create = 16,

    /// Запрет чтения
    cant_read   = 32,

    /// Запрет обновления
    cant_update = 64,

    /// Запрет удаления
    cant_delete = 128
}

Access[] access_list =
[
    Access.can_create, Access.can_read, Access.can_update, Access.can_delete
];

Access[] denied_list =
[
    Access.cant_create, Access.cant_read, Access.cant_update, Access.cant_delete
];

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

public enum INDV_OP : byte
{
    /// Сохранить
    PUT         = 1,

    /// Сохранить
    GET         = 2,

    /// Получить тикет
    GET_TICKET  = 3,

    /// Авторизовать
    AUTHORIZE   = 8,

    /// Установить в
    SET_IN      = 45,

    /// Добавить в
    ADD_IN      = 47,

    /// Убрать из
    REMOVE_FROM = 48,

    /// Убрать
    REMOVE      = 51
}


/// Команды используемые процессами
/// Сохранить
byte CMD_PUT         = 1;

/// Найти
byte CMD_FIND        = 2;

/// Получить
byte CMD_GET         = 2;

/// Проверить
byte CMD_EXAMINE     = 4;

/// Авторизовать
byte CMD_AUTHORIZE   = 8;

/// Коммит
byte CMD_COMMIT      = 16;

byte CMD_MSG         = 17;

/// Конец данных
byte CMD_END_DATA    = 32;

/// Включить/выключить отладочные сообщения
byte CMD_SET_TRACE   = 33;

/// Выгрузить
byte CMD_UNLOAD      = 34;

/// Перезагрузить
byte CMD_RELOAD      = 40;

/// Backup
byte CMD_BACKUP      = 41;

/// Остановить прием команд на изменение
byte CMD_FREEZE      = 42;

/// Возобновить прием команд на изменение
byte CMD_UNFREEZE    = 43;

/// Установить в
byte CMD_SET_IN      = 45;

/// Удалить
byte CMD_DELETE      = 46;

/// Добавить в
byte CMD_ADD_IN      = 47;

/// Убрать из
byte CMD_REMOVE_FROM = 48;


byte CMD_EXIT        = 49;

/// Установить
byte CMD_SET         = 50;

/// Убрать
byte CMD_REMOVE      = 51;

byte CMD_START       = 52;

byte CMD_STOP        = 53;

byte CMD_RESUME      = 54;

byte CMD_PAUSE       = 55;

byte CMD_WAIT        = 56;

/// Пустая комманда
byte   CMD_NOP       = 64;

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
