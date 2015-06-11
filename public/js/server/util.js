// Server-side utility functions
"use strict";

/// Создание
var can_create = 1;

/// Чтение
var can_read = 2;

/// Изменеие
var can_update = 4;

/// Удаление
var can_delete = 8;

/// Запрет создания
var cant_create = 16;

/// Запрет чтения
var cant_read = 32;

/// Запрет обновления
var cant_update = 64;

/// Запрет удаления
var cant_delete = 128;

function toJson(x)
{
    return JSON.stringify(x, null, 2);
}

function newDocument(type, fields)
{

}

function newUri(uri)
{
    return [
        {
            data: uri,
            type: _Uri
    }];
}

function newStr(_data)
{
    return [
        {
            data: _data,
            type: _String
    }];
}

function getUri(field)
{
    if (field && field.length > 0)
    {
        return field[0].data;
    }
}

function getFirstValue(field)
{
    if (field && field.length > 0)
    {
        return field[0].data;
    }
}

function addRight(ticket, rights, subj_uri, obj_uri)
{
    var c = false,
        r = false,
        d = false,
        u = false;

    for (var i = 0; i < rights.length; i++)
    {
        if (rights[i] == can_read)
            r = true;
        else if (rights[i] == can_update)
            u = true;
        else if (rights[i] == can_delete)
            d = true;
        else if (rights[i] == can_create)
            c = true;
    }

    var new_permission = {
        '@': guid(),
        'rdf:type': [
            {
                data: 'v-s:PermissionStatement',
                type: _Uri
        }],
        'v-s:canDelete': [
            {
                data: d,
                type: _Bool
        }],
        'v-s:canRead': [
            {
                data: r,
                type: _Bool
        }],
        'v-s:canUpdate': [
            {
                data: u,
                type: _Bool
        }],
        'v-s:permissionObject': [
            {
                data: obj_uri,
                type: _Uri
        }],
        'v-s:permissionSubject': [
            {
                data: subj_uri,
                type: _Uri
        }]
    };
    put_individual(ticket, new_permission, _event_id);

    //print("ADD RIGHT:", toJson(new_permission));
}
