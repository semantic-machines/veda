// Server-side utility functions
"use strict";

function toJson(x)
{
    return JSON.stringify(x, null, 2);
}

function hasValue(doc, prop, val) {
	var any = !!(doc[prop] && doc[prop].length);
	if (!val) return any;
	return !!(any && doc[prop].filter(function (i) {
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
	//	print("@@@ logToJournal, new_journal_record=" + toJson(journal_record));
	
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
	//	print("@@@ logToJournal, add_to_journal = " + toJson(add_to_journal));

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

function isTecnicalChange(newdoc, olddoc) {
	if (newdoc['v-s:actualVersion'] && newdoc['v-s:actualVersion'][0].data != newdoc['@']) {
		olddoc = get_individual(ticket, newdoc['v-s:actualVersion'][0].data);
	}
	if (!olddoc) {
		// print (newdoc['@']+' x ');
		return false;
	}

	for (var key in newdoc) {
		if (key === '@') continue;
		
		if ((newdoc[key] && !olddoc[key])  // добвили
		     || (newdoc[key] && !olddoc[key]) // удалили
		     || (newdoc[key].length !== olddoc[key].length) // изменили количество
		    ) 
		{ 	
			if (!isTechnicalAttribute(key, olddoc[key])) {
				// в нетехническом атрибуте
				//print (newdoc['@']+' x '+olddoc[key]+' >1> '+newdoc[key]+' : '+key);
				return false;				
			}
		} else {
			for (var item in newdoc[key]) {
				if (newdoc[key][item].data.valueOf() != olddoc[key][item].data.valueOf() && !isTechnicalAttribute(key, olddoc[key][item].data)) { // поменялось одно из значений в нетехническом атрибуте
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

function isTechnicalAttribute(attName, oldvalue) {
	if (!oldvalue && attName === 'v-s:actualVersion') return true;
	if (!oldvalue && attName === 'v-s:previousVersion') return true;
	if (!oldvalue && attName === 'v-s:nextVersion') return true;
	if (attName === 'v-s:isDraftOf') return true;
	if (attName === 'v-s:hasDraft') return true;
	if (attName === 'v-s:hasStatusWorkflow') return true;
	return false;
}

function loadVariablesUseField (ticket, field)
{
	var res = {};
	for (var idx in field)
	{
		var uri = field[idx].data;
		if (uri)
		{
			var indv = get_individual (ticket, uri);
			
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

function isAlphaNumeric (src)
{
	if (!src)
		return false;
	var alphanum=/[a-zA-Z0-9]/;
	if(alphanum.test(src))
		return true;
	else
		return false;
}		

function replace_word(src, from, to)
{
	var trace = false;
	
//	if (src === 'd:membership_v-wf_Variable_cfg_OntologyGroup')
//		trace = true;
	
    var new_str = src;
    
    if (trace)
		print ('src=', src, ', from=', from, ', to=', to); 
		
    var is_prepare = false;

    if (src.length == from.length)
        is_prepare = true;

    if (is_prepare == false)
    {
        var pos = src.indexOf(from);
        if (pos && pos >= 0)
        {
			if (trace)
			{
				print ('src=', src, ', from=', from, ', to=', to); 
				print ('$ #1 pos=', pos);
			}	

            var last_ch = src[pos + from.length];
            
            if (trace)
				print ('$ #2 last_ch=[' + last_ch + ']');

            if (last_ch && isAlphaNumeric(last_ch) == false)
            {
				if (trace)
				{
					print ('$ !isAlphaNumeric last_ch=', last_ch);
				}
                is_prepare = true;
            }
        }
    }

    if (is_prepare)
    {
        new_str = src.replace(new RegExp(from, 'g'), to);
    }

    return new_str;
}
