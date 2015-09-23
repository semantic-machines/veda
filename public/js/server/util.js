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

function newBool(_data)
{
    return [
        {
            data: _data,
            type: _Bool
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
        '@': genUri(),
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

/////////////////////////////////////// JOURNAL

function getJournalUri(object_uri)
{
    return object_uri + "j";
}

function getTraceJournalUri(object_uri)
{
    return object_uri + "t";
}

function newJournalRecord(journal_uri)
{
    var new_journal_record_uri = genUri();

    var new_journal_record = {
        '@': new_journal_record_uri,
        'rdf:type': [
            {
                data: 'v-s:JournalRecord',
                type: _Uri
    }],
        'v-s:parentJournal': [
            {
                data: journal_uri,
                type: _Uri
    }],
        'v-s:created': [
            {
                data: new Date (),
                type: _Datetime
    }]	
    };
    return new_journal_record;
}

function logToJournal(ticket, journal_uri, journal_record)
{
    //print("new_journal_record=" + toJson(new_journal_record));
    put_individual(ticket, journal_record, _event_id);

    var add_to_journal = {
        '@': journal_uri,
        'v-s:childRecord': [
            {
                data: journal_record['@'],
                type: _Uri
    }]
    };

    add_to_individual(ticket, add_to_journal, _event_id);
}

function traceToJournal(ticket, journal_uri, label, data)
{
	var journal_record = newJournalRecord(journal_uri);
	
	journal_record['rdf:type'] = [
            {
                data: 'v-wf:TraceRecord',
                type: _Uri
    }];	
	journal_record['rdfs:label'] = [
            {
                data: label,
                type: _String
    }];
	journal_record['rdfs:comment'] = [
            {
                data: data,
                type: _String
    }];
    
	logToJournal(ticket, journal_uri, journal_record);		
}	

