// Common utility functions

"use strict";

var _Uri      = 1;
var _String   = 2;
var _Integer  = 4;
var _Datetime = 8;
var _Decimal  = 32;
var _Bool     = 64;


function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
               .toString(16)
               .substring(1);
  }
  return 'd' + s4() + s4() + s4() + s4() + s4() + s4() + s4() + s4();
}

function compare (a, b) {
	if (typeof a === "function") return a.toString() === b.toString();
	else if (typeof a != "object" || typeof b != "object") return a === b;
	if (Object.keys(a).length != Object.keys(b).length) return false;
	var result = true;
	for (var key in a) 
	{
		var bb = b[key];
		var aa = a[key];

		var tbb = typeof bb;
		var taa = typeof aa;

		if (key == "type")
		{
		    if (tbb == 'number' && taa == 'string')
		    {
		    if (bb == _Uri)
			bb = 'Uri';
		    else if (bb == _String)
			bb = 'String';
		    else if (bb == _Integer)
			bb = 'Integer';
		    else if (bb == _Datetime)
			bb = 'Datetime';
		    else if (bb == _Decimal)
			bb = 'Decimal';
		    else if (bb == _Bool)
			bb = 'Boolean';
		    }				    
		    else if (taa == 'number' && tbb == 'string')
		    {
		    if (aa == _Uri)
			aa = 'Uri';
		    else if (aa == _String)
			aa = 'String';
		    else if (aa == _Integer)
			aa = 'Integer';
		    else if (aa == _Datetime)
			aa = 'Datetime';
		    else if (aa == _Decimal)
			aa = 'Decimal';
		    else if (aa == _Bool)
			aa = 'Boolean';
		    }
		}

		result &= compare(aa, bb);
		if (!result) return false;
	}
	return result;
}

function sleep(usec) {
	var endtime= new Date().getTime() + usec;
    while (new Date().getTime() < endtime);
}

var ticket_manager             = 0;
var subject_manager 	       = 1;
var acl_manager                = 2;
var fulltext_indexer           = 4;
var condition                  = 6;

function get_property_chain (ticket, first, rest) 
{ 
	var doc;
	doc = typeof first == "object" ? first : get_individual (ticket, first);

//	print ('@js ------------------');
//	print ('@js #1 doc=', toJson (doc));;

	var doc_first = doc;
	var field;

	for (var i = 1; i < arguments.length; i++) 
	{
		field = doc[arguments[i]];
		if (field && (field[0].type == "Uri" || field[0].type == _Uri))
		{
			doc = get_individual (ticket, field[0].data); 	
//			print ('@js #2 doc=', toJson (doc));;
			if (!doc) break;
		}
	}		

	var res = {};

	if (field !== undefined)
	{
	    res.field = field;
	    res.first = doc_first;
	    res.last = doc;
	}
	return res;	
}

function is_exist (individual, field, value)
{
        var ff = individual[field];
	if (ff)
	{
	    for (var i = 0; i < ff.length; i++) 
	    {
		if (ff[i].data == value)
		    return true; 
	    }
	}
	return false;
}
