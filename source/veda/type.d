/**
 * Общие определения

   Copyright: © 2014-2015 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev
 */
module veda.type;

import std.math, std.stdio, std.conv, std.string;

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

/// Команды используемые процессами
enum CMD : byte
{
    /// Сохранить
    PUT          = 1,

    /// Найти
    FIND         = 2,

    /// Получить
    GET          = 2,

    /// Проверить
    EXAMINE      = 4,

    /// Авторизовать
    AUTHORIZE    = 8,

    /// Коммит
    COMMIT       = 16,

    /// Конец данных
    END_DATA     = 32,

    /// Вклчить/выключить отладочные сообщения
    SET_TRACE    = 33,

    /// Перезагрузить
    RELOAD       = 40,

    /// Backup
    BACKUP       = 41,

    /// Остановить прием команд на изменение
    FREEZE       = 42,

    /// Возобновить прием команд на изменение
    UNFREEZE     = 43,

    /// Сохранить соответствие ключ - слот (xapian)
    PUT_KEY2SLOT = 44,

    /// Установить
    SET          = 45,

    /// Удалить
    DELETE       = 46,

    /// Добавить
    ADD          = 47,

    /// Убрать
    REMOVE       = 48,

    /// Пустая комманда
    NOP          = 64,


    EXIT         =  49
}


/// Десятичное число
struct decimal
{
    /// мантисса
    long mantissa;

    /// экспонента
    long exponent;

    /// конструктор
    this(long m, long e)
    {
        mantissa = m;
        exponent = e;
    }

    /// конструктор
    this(string num)
    {
        string[] ff = split(num, ".");

        if (ff.length == 2)
        {
            long sfp = ff[ 1 ].length;

            mantissa = to!long (ff[ 0 ] ~ff[ 1 ]);
            exponent = -sfp;
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
        exponent = -count;
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
}
