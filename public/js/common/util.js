// Common utility functions

"use strict";

var _Uri      = 1;
var _String   = 2;
var _Integer  = 4;
var _Datetime = 8;
var _Decimal  = 32;
var _Bool     = 64;

function genUri()
{
    return 'd:' + guid();
}

function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
               .toString(16)
               .substring(1);
  }
  return s4() + s4() + s4() + s4() + s4() + s4() + s4() + s4();
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

/**
 * Трансформировать указанные индивидуалы по заданным правилам
 * 
 * @param ticket сессионный билет 
 * @param individuals один или несколько IndividualModel или их идентификаторов
 * @param transform применяемая трансформация 
 * @param executor контекст исполнителя
 * @param work_order контекст рабочего задания
 * @returns {Array}
 */
function transformation(ticket, individuals, transform, executor, work_order)
{
    var out_data0 = {};

    if (Array.isArray(individuals) !== true) {
    	individuals = [individuals];
    }

    var rules = transform['v-wf:transformRule'];

    if (!rules)
        return;

    for (var i in rules)
    {
    	if (typeof window === "undefined") {
    		rules[i] = get_individual(ticket, rules[i].data);
    	} else {
    		rules[i] = new veda.IndividualModel(rules[i].data);
    	}
    }

    //print("#6 individuals=", toJson(individuals));
    var out_data0_el = {};

    individuals.forEach(function(individual)
    {
        var objectContentStrValue = (function ()
        {
            return function (name, value)
            {
            	if (individual[name])                     	
            	{
	            	var result = false;
	            	if (typeof window === "undefined") {            	
	            		for (var i in individual[name]) {
	            			if (value === individual[name][i].data) {
	            				result = true; 
	            			}
	            		}
	            	} else {
	            		for (var i in individual[name]) {
	            			if (value === individual[name][i]) {
	            				result = true; 
	            			}
	            		}
	            	}
	            	return result;
            	}
            	/*
                //print("individual[name]=", toJson(individual[name]));
                var str = individual[name][0].data;
                //print("str=", str);
                if (str == value)
                    return true;
                else
                    return false;
                    */
            }
        })();

        for (var key in individual)
        {
            var element = individual[key];

            var contentName = (function ()
            {
                return function (name)
                {
                    if (key == name)
                        return true;
                    else
                        return false;
                }
            })();

            // выполняем все rules
            rules.forEach(function(rule)
            {
                // 1. v-wf:segregateObject
                var segregateObject = rule['v-wf:segregateObject'];

                // 2. v-wf:segregateElement
                var segregateElement = rule['v-wf:segregateElement'];
                var grouping = rule['v-wf:grouping'];

                var res = false;

                if (segregateObject)
                {
                    res = eval(typeof window === "undefined"?segregateObject[0].data:segregateObject[0]);
                    if (res == false)
                        return;
                }

                var elementContentStrValue = (function ()
                {
                    return function (name, value)
                    {
                        if (key !== name)
                            return false;
                        //print("individual[name]=", toJson(individual[name]));
                        var str = typeof window === "undefined"?element[0].data:element[0];
                        //print("str=", str);
                        if (str == value)
                            return true;
                        else
                            return false;
                    }
                })();

                if (segregateElement)
                {
                    res = eval(typeof window === "undefined"?segregateElement[0].data:segregateElement[0]);
                    if (res == false)
                    	return;
                }

                var getElement = (function ()
                {
                    return function ()
                    {
                        return element;
                    }
                })();

                var putFieldOfIndividFromElement = (function ()
                {
                    return function (name, field)
                    {                    	
                        var rr = get_individual(ticket, getUri(element));
                        if (!rr)
                            return;

                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        out_data0_el_arr.push(rr[field]);

                        out_data0_el[name] = out_data0_el_arr;
                    }
                })();

                var putElement = (function ()
                {
                    return function (name)
                    {
                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        if (key == '@')
                        {
                            out_data0_el_arr.push(
                            {
                                data: element,
                                type: _Uri
                            });
                        }
                        else
                            out_data0_el_arr.push(element);

                        out_data0_el[name] = out_data0_el_arr;
                    }
                })();

                var putFieldOfObject = (function ()
                {
                    return function (name, field)
                    {
                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        out_data0_el_arr.push(individual[field]);

                        out_data0_el[name] = out_data0_el_arr;
                    }
                })();

                var putUri = (function ()
                {
                    return function (name, value)
                    {
                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        out_data0_el_arr.push(
                        {
                            data: value,
                            type: _Uri
                        });

                        out_data0_el[name] = out_data0_el_arr;
                    }
                })();

                var putString = (function ()
                {
                    return function (name, value)
                    {
                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        out_data0_el_arr.push(
                        {
                            data: value,
                            type: _String
                        });

                        out_data0_el[name] = out_data0_el_arr;
                    }
                })();

                var putBoolean = (function ()
                {
                    return function (name, value)
                    {
                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        out_data0_el_arr.push(
                        {
                            data: value,
                            type: _Bool
                        });

                        out_data0_el[name] = out_data0_el_arr;
                    }
                })();

                var putExecutor = (function ()
                {
                    return function (name)
                    {
                        out_data0_el[name] = [executor];
                    }
                })();

                var putWorkOrder = (function ()
                {
                    return function (name)
                    {
                        out_data0_el[name] = [work_order];
                    }
                })();

                //print("#7 key=", key);
                //print("#7 element=", toJson(element));

                //print("#9 segregateElement=", segregateElement[0].data);

                // 3. v-wf:aggregate
                var group_key;
                if (!grouping)
                {
                    out_data0_el = {};
                    out_data0_el['@'] = genUri();
                }
                else
                {
                    group_key = typeof window === "undefined"?grouping[0].data:grouping[0];
                    out_data0_el = out_data0[group_key];
                    if (!out_data0_el)
                    {
                        out_data0_el = {};
                        out_data0_el['@'] = genUri();
                    }
                }

                var agregate = rule['v-wf:aggregate'];
                for (var i2 = 0; i2 < agregate.length; i2++)
                {
                    eval(typeof window === "undefined"?agregate[i2].data:agregate[i2]);
                }

                if (!grouping)
                {
                    out_data0[out_data0_el['@']] = out_data0_el;
                }
                else
                {
                    out_data0[group_key] = out_data0_el;
                }
            });
        }
    });

    var out_data = [];
    for (var key in out_data0)
    {
        out_data.push(out_data0[key]);
    }

    //print ("#8 out_data=", toJson (out_data));

    return out_data;
}