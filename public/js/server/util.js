// Server-side utility functions
"use strict";

function toJson(x)
{
    return JSON.stringify(x, null, 2);
}

function hasValue(doc, prop, val)
{
    var any = !!(doc && doc[prop] && doc[prop].length);
    if (!val) return any;
    return !!(any && doc[prop].filter(function(i)
    {
        return (i.type === val.type && i.data === val.data);
    }).length);
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
            data: new Date(),
            type: _Datetime
        }]
    };
    return new_journal_record;
}

function logToJournal(ticket, journal_uri, journal_record, jr_type)
{
    //if (!jr_type)
    //  print("@@@ logToJournal, new_journal_record=" + toJson(journal_record));

    put_individual(ticket, journal_record, _event_id);

    var add_to_journal = {
        '@': journal_uri,
        'v-s:childRecord': [
        {
            data: journal_record['@'],
            type: _Uri
        }]
    };

    //if (!jr_type)
    //  print("@@@ logToJournal, add_to_journal = " + toJson(add_to_journal));

    //var before = get_individual(ticket, journal_uri);
    //print('BEFORE : '+toJson(before))

    add_to_individual(ticket, add_to_journal, _event_id);

    //var after = get_individual(ticket, journal_uri);
    //print('AFTER : '+toJson(after))
}

function traceToJournal(ticket, journal_uri, label, _data)
{
    //print("@@@ traceToJournal, journal_uri=" + journal_uri + " #1");
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
        data: _data,
        type: _String
    }];

    logToJournal(ticket, journal_uri, journal_record, true);

    //print("@@@ traceToJournal, journal_uri=" + journal_uri + ", " + toJson(journal_record));
}

function isTecnicalChange(newdoc, olddoc)
{
    if (newdoc['v-s:actualVersion'] && newdoc['v-s:actualVersion'][0].data != newdoc['@'])
    {
        olddoc = get_individual(ticket, newdoc['v-s:actualVersion'][0].data);
    }
    if (!olddoc)
    {
        // print (newdoc['@']+' x ');
        return false;
    }

    for (var key in newdoc)
    {
        if (key === '@') continue;

        if ((newdoc[key] && !olddoc[key]) // добвили
            ||
            (newdoc[key] && !olddoc[key]) // удалили
            ||
            (newdoc[key].length !== olddoc[key].length) // изменили количество
        )
        {
            if (!isTechnicalAttribute(key, olddoc[key]))
            {
                // в нетехническом атрибуте
                //print (newdoc['@']+' x '+olddoc[key]+' >1> '+newdoc[key]+' : '+key);
                return false;
            }
        }
        else
        {
            for (var item in newdoc[key])
            {
                if (newdoc[key][item].data.valueOf() != olddoc[key][item].data.valueOf() && !isTechnicalAttribute(key, olddoc[key][item].data))
                { // поменялось одно из значений в нетехническом атрибуте
                    //print ('2 old:', toJson(olddoc));
                    //print ('2 new:', toJson(newdoc));
                    //print (newdoc['@']+' x '+olddoc[key][item].data+' >2> '+newdoc[key][item].data+' : '+key);
                    return false;
                }
            }
        }
    }

    return true;
}

function isTechnicalAttribute(attName, oldvalue)
{
    if (!oldvalue && attName === 'v-s:actualVersion') return true;
    if (!oldvalue && attName === 'v-s:previousVersion') return true;
    if (!oldvalue && attName === 'v-s:nextVersion') return true;
    if (attName === 'v-s:isDraftOf') return true;
    if (attName === 'v-s:hasDraft') return true;
    if (attName === 'v-wf:hasStatusWorkflow') return true;
    return false;
}

function loadVariablesUseField(ticket, field)
{
    var res = {};
    for (var idx in field)
    {
        var uri = field[idx].data;
        if (uri)
        {
            var indv = get_individual(ticket, uri);

            if (is_exist(indv, 'rdf:type', 'v-s:Variable'))
            {
                var varName = getFirstValue(indv['v-s:variableName']);
                var varValue = getStrings(indv['v-s:variableValue']);
                res[varName] = varValue;
            }
        }
    }
    return res;
}

function isAlphaNumeric(src)
{
    if (!src)
        return false;
    var alphanum = /[a-zA-Z0-9]/;
    if (alphanum.test(src))
        return true;
    else
        return false;
}

function replace_word(src, from, to)
{
    var trace = true;

    var new_str = src;
    //if (trace)
    //  print ('src=', src, ', from=', from, ', to=', to);

    var is_prepare = false;

    var pos_f = from.indexOf('*');
    var pos_t = to.indexOf('*');

    if (pos_f > 0 && pos_f > 0)
    {
        from = from.substring(0, pos_f);
        to = to.substring(0, pos_t);

        var pos_w_b = src.indexOf(from);
        var word;
        if (pos_w_b >= 0)
        {
            pos_w_b += from.length;
            var pos_w_e = pos_w_b;
            var ch = src.charAt(pos_w_e);
            while (isAlphaNumeric(ch))
            {
                pos_w_e++;
                ch = src.charAt(pos_w_e);
            }
            if (pos_w_e > pos_w_b)
            {
                word = src.substring(pos_w_b, pos_w_e);
                //print ('is *1, from=', from, ", to=", to);
                //print ('is *2, word=', word);
                from = from + word;
                to = to + word;
                //print ('is *3, from=', from, ", to=", to);

                is_prepare = true;
            }
        }
    }
    else
    {
        if (src.length == from.length)
            is_prepare = true;

        if (is_prepare == false)
        {
            var pos = src.indexOf(from);
            if (pos && pos >= 0)
            {
                if (trace)
                {
                    print('$replace_word #1 pos=', pos);
                }

                var last_ch = src[pos + from.length];

                if (trace)
                    print('$replace_word #2 last_ch=[' + last_ch + ']');

                if (last_ch && isAlphaNumeric(last_ch) == false)
                {
                    if (trace)
                    {
                        print('$replace_word !isAlphaNumeric last_ch=', last_ch);
                    }
                    is_prepare = true;
                }
            }
        }
    }

    if (is_prepare)
    {
        new_str = src.replace(new RegExp(from, 'g'), to);
    }


    return new_str;
}

/**
 * Create document snapshot
 * @param ticket
 * @param document Document
 * @param prev_state Previous document state
 * @param user_uri Initiator
 * @param _event_id
 */
function create_version(ticket, document, prev_state, user_uri, _event_id) {
  // Only if we save actual version of document (or it is first save of versioned document)
  if (!document['v-s:actualVersion']
    || (document['v-s:actualVersion'][0].data == document['@']
      && ( ( !document['v-s:previousVersion'] && (!prev_state || !prev_state['v-s:previousVersion']) )
          || (prev_state
            && document['v-s:previousVersion']
            && prev_state['v-s:previousVersion']
            && document['v-s:previousVersion'][0].data == prev_state['v-s:previousVersion'][0].data)
         )
       )
     ) {
      if (!prev_state) prev_state = document;
      var actualId = document['@'];
    var versionId = genUri();

    // Create new version
    var version = get_individual(ticket, document['@']);
    version['@'] = versionId;
    if (prev_state['v-s:previousVersion']) {
      version['v-s:previousVersion'] = prev_state['v-s:previousVersion'];
    } else {
      version['v-s:previousVersion'] = [];
    }
    version['v-s:actualVersion'] = [{
          data: document['@'],
          type: _Uri
    }];
    version['v-s:nextVersion'] = [{
      data: actualId,
      type: _Uri
    }];
    version['rdf:type'] = version['rdf:type'].concat(
      [{
        data: "v-s:Version",
        type: _Uri
      }]
    );
    version['v-s:created'] = [{data: new Date(), type: _Datetime}];
    version['v-s:edited'] = [];
    version['v-s:creator'] = newUri(user_uri);
    version['v-s:lastEditor'] = [];

    put_individual(ticket, version, _event_id);

    // Add rights to version
    var uri = 'd:membership_' + versionId.split(':').join('_') + '_' + actualId.split(':').join('_');

    var membership = get_individual(ticket, uri);

    if (!membership) {
      //print('+M '+uri);
      membership = {
        '@' : uri,
        'rdf:type'     : newUri('v-s:Membership'),
        'v-s:memberOf' : newUri(actualId),
        'v-s:resource' : newUri(versionId),
        'rdfs:comment' : newStr('создано cfg:Event_3'),
        'v-s:canRead'  : newBool(true)
      };
      put_individual (ticket, membership, _event_id);
    }

    // Update previous version
    if (document['v-s:previousVersion']) {
      var previous = get_individual(ticket, getUri(document['v-s:previousVersion']));
      previous['v-s:nextVersion'] = [{
            data: versionId,
            type: _Uri
      }];
      put_individual(ticket, previous, _event_id);
    }

    // Update actual version
    document['v-s:previousVersion'] = [{
          data: versionId,
          type: _Uri
    }];
    document['v-s:actualVersion'] = [{
          data: document['@'],
          type: _Uri
    }];
    document['v-s:nextVersion'] = [];
    document['v-s:edited'] = [{data: new Date(), type: _Datetime}];
    document['v-s:lastEditor'] = newUri(user_uri);
    put_individual(ticket, document, _event_id);
  }
}

function recursiveCall(elem, path, ticket, _event_id) {  
    if (path[elem['@']]) {
        print('WARNING! Recursive path '+toJson(path)+' > '+elem['a']);     
        return;
    }
    
    path[elem['@']] = Object.keys(path).length;
    if (elem['v-wf:decisionFormList']) {
      elem['v-wf:decisionFormList'].forEach(function(dfae) {
        var df = get_individual(ticket, dfae.data)
        if (!df['v-wf:isCompleted'] ||
            df['v-wf:isCompleted'][0].data == false)
          {
          df['v-s:deleted'] = newBool(true);
          df['v-wf:isStopped'] = newBool(true);
          put_individual(ticket, df, _event_id);
        }
      });
    }

    if (elem['v-wf:workItemList']) {
      elem['v-wf:workItemList'].forEach(function(wi) {
        recursiveCall(get_individual(ticket, wi.data), path, ticket, _event_id);
      });
    }

    if (elem['v-wf:workOrderList']) {
      elem['v-wf:workOrderList'].forEach(function(wo) {
        recursiveCall(get_individual(ticket, wo.data), path, ticket, _event_id);
      });
    }

    if (elem['v-wf:isProcess']) {
      elem['v-wf:isProcess'].forEach(function(p) {
        recursiveCall(get_individual(ticket, p.data), path, ticket, _event_id);
      });
    }
}
