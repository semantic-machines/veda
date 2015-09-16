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
    return 'd:a' + guid();
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
  try {	
    var out_data0 = {};

    if (Array.isArray(individuals) !== true) {
    	individuals = [individuals];
    }

    var rules = transform['v-wf:transformRule'];

    if (!rules)
        return;

	if (typeof window === "undefined") 
	{
		for (var i in rules)
		{
    		rules[i] = get_individual(ticket, rules[i].data);
    		if (!rule)
			{
				print ("not read rule [", rules[i].data, "]");
				continue;
			}				
    	}
    }

    var out_data0_el = {};
    
    /* PUT functions [BEGIN] */
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
    /* PUT functions [END] */

    for (var key in individuals)
    {
    	//print("#1 key=", key);
    	var individual = individuals[key];
    	if (typeof window !== "undefined") individual['@'] = individual['id']; // sly hack
    	
    	//print("#1.1 key=", key);
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

        //print("#1.2 key=", key);
        var iteratedObject = (typeof window === "undefined")?Object.getOwnPropertyNames(individual):Object.getOwnPropertyNames(individual.properties);
        if (typeof window !== "undefined") {
        	iteratedObject.push('@');
        }
        //print("#1.3 key=", key);

        for (var key2 = 0; key2 < iteratedObject.length; key2++)
        {
        	//print("#2 key2=", key2);
            var element = individual[iteratedObject[key2]];

            var putElement = (function ()
    	    {
    	        return function (name)
    	        {
    	            var out_data0_el_arr;

    	            out_data0_el_arr = out_data0_el[name];

    	            if (!out_data0_el_arr)
    	                out_data0_el_arr = [];
    	            if (iteratedObject[key2] == '@')
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
            
            /* Segragate functions [BEGIN] */
            var contentName = (function ()
            {
                return function (name)
                {
                	return iteratedObject[key2] == name;
                }
            })();

            var elementContentStrValue = (function ()
            {
                return function (name, value)
                {
                    if (iteratedObject[key2] !== name)
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
            /* Segragate functions [END] */
            
            var getElement = (function ()
            {
                return function ()
                {
                    return element;
                }
            })();


            // выполняем все rules
            for (var key3 in rules)
            {
            	//print("#3 key3=", key3);
            	var rule = rules[key3];
                // 1. v-wf:segregateObject
                var segregateObject = rule['v-wf:segregateObject'];

                // 2. v-wf:segregateElement
                var segregateElement = rule['v-wf:segregateElement'];
                var grouping = rule['v-wf:grouping'];

                var res = undefined;
                
                if (segregateObject)
                {
                	if (typeof window === "undefined") {
                		res = eval(segregateObject[0].data)
                	} else if (segregateObject[0] != undefined) {
                		res = eval(segregateObject[0].toString());
                	}
                    if (res == false)
                    	continue;
                }

                if (segregateElement)
                {
                	if (typeof window === "undefined") {
                		res = eval(segregateElement[0].data)
                	} else if (segregateElement[0] != undefined) {
                		res = eval(segregateElement[0].toString());
                	}
                    if (res == false)
                    	continue;
                }

                //print("#7 key=", key);
                //print("#7 element=", toJson(element));

				//if (segregateElement)
				//	print("#8 segregateElement=", segregateElement[0].data);

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
                	if (typeof window === "undefined") 
                	{
                		eval(agregate[i2].data);
                	} else if (agregate[i2] != undefined) 
                	{ 
                		eval(agregate[i2].toString());
                	}
                }

                if (!grouping)
                {
                    out_data0[out_data0_el['@']] = out_data0_el;
                }
                else
                {
                    out_data0[group_key] = out_data0_el;
                }
            }          
        }
    }

    var out_data = [];
    for (var key in out_data0)
    {
        out_data.push(out_data0[key]);
    }

    //print ("#e out_data=", toJson (out_data));

    return out_data;
  } catch(e) {
	if (typeof window === "undefined") {
		print(e.stack);
	} else {
		console.log(e.stack);
	}
  }
}
