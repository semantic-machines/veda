/**
 * core type
 */
module veda.core.common.type;

import veda.common.type;

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

