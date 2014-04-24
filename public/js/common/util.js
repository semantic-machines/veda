// Common utility functions

"use strict";

function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
               .toString(16)
               .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
         s4() + '-' + s4() + s4() + s4();
}

function compare (a, b) {
	if (typeof a === "function") return a.toString() === b.toString();
	else if (typeof a != "object" || typeof b != "object") return a === b;
	if (Object.keys(a).length != Object.keys(b).length) return false;
	var result = true;
	for (var key in a) {
		result &= compare(a[key], b[key]);
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
	doc = typeof first === "object" ? first : get_individual (ticket, first);
	var doc_first = doc;
	var field;

	for (var i = 1; i < arguments.length; i++) 
	{
		field = doc[arguments[i]];
		if (field && field[0].type == "Uri")
		{
			doc = get_individual (ticket, field[0].data); 	
			if (!doc) break;
		}
	}		

	var res = {};

	if (field != undefined)
	{
	    res.field = field;
	    res.first = doc_first;
	    res.last = doc;
	}
	return res;	
}