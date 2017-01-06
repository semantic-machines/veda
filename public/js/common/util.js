// Common utility functions
"use strict";

var _Uri = 1;
var _String = 2;
var _Integer = 4;
var _Datetime = 8;
var _Decimal = 32;
var _Bool = 64;
var _Boolean = 64;

function removeV(arr, what) {
  var res = [];
  print ("@b in=", toJson (arr));
  for (var i = 0; i < arr.length; i++)
  {
    if (what.data != arr[i].data)
      res = arr[i];
  }
  print ("@e out=", toJson (res));
  return res;
}

function genUri()
{
  var uid = guid();
  if (uid[0] == '0' || uid[0] == '1' || uid[0] == '2' || uid[0] == '3' || uid[0] == '4' || uid[0] == '5' || uid[0] == '6' || uid[0] == '7' || uid[0] == '8' || uid[0] == '9')
    return 'd:a' + uid;
  else
    return 'd:' + uid;
}

function guid()
{
  function s4()
  {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(36)
      .substring(1);
  }
  return s4() + s4() + s4() + s4() + s4() + s4() + s4() + s4();
}

function compare(a, b)
{
  if (typeof a === "function") return a.toString() === b.toString();
  else if (typeof a != "object" || typeof b != "object") return a === b;
  var dl = Object.keys(a).length - Object.keys(b).length;
  if (dl > 1 || dl < -1) return false;
  var result = true;
  for (var key in a)
  {
    var bb = b[key];
    var aa = a[key];

    var tbb = typeof bb;
    var taa = typeof aa;

    if (key == "v-s:updateCounter")
      continue;

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
    else(key == "lang")
    {
      if (tbb == 'number' && taa == 'string')
      {
        if (bb == 0)
          bb = 'NONE';
      }
      else if (taa == 'number' && tbb == 'string')
      {
        if (aa == 0)
          aa = 'NONE';
      }
    }
    result &= compare(aa, bb);
    if (!result) return false;
  }
  return result;
}

function sleep(usec)
{
  var endtime = new Date().getTime() + usec;
  while (new Date().getTime() < endtime);
}

var ticket_manager = 0;
var subject_manager = 1;
var acl_manager = 2;
var fulltext_indexer = 4;
var condition = 6;

function get_property_chain(ticket, first, rest)
{
  var doc;
  doc = typeof first == "object" ? first : get_individual(ticket, first);

  //  print ('@js ------------------');
  //  print ('@js #1 doc=', toJson (doc));;

  var doc_first = doc;
  var field;

  for (var i = 1; i < arguments.length; i++)
  {
    field = doc[arguments[i]];
    if (field && (field[0].type == "Uri" || field[0].type == _Uri))
    {
      doc = get_individual(ticket, field[0].data);
      //      print ('@js #2 doc=', toJson (doc));;
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

function is_exist(individual, field, value)
{
  if (!individual)
    return false;
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
function transformation(ticket, individuals, transform, executor, work_order, process)
{
  try
  {
    var out_data0 = {};

    if (Array.isArray(individuals) !== true)
    {
      individuals = [individuals];
    }

    var rules = transform['v-wf:transformRule'];

    if (!rules)
      return;

    if (typeof window === "undefined")
    {
      //print ("@B start transform");
      var tmp_rules = [];
      //print ("rules_in=", toJson (rules));
      //print ("individuals=", toJson (individuals));
      for (var i in rules)
      {
        var rul = get_individual(ticket, rules[i].data);
        if (!rul)
        {
          print("not read rule [", toJson(rul), "]");
          continue;
        }
        else
          tmp_rules.push(rul);
      }
      rules = tmp_rules;
    }

    var out_data0_el = {};

    /* PUT functions [BEGIN] */
    var putFieldOfIndividFromElement = (function()
    {
      return function(name, field)
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

    var putFieldOfObject = (function()
    {
      return function(name, field)
      {
        var out_data0_el_arr;

        out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
            out_data0_el_arr = [];

        out_data0_el_arr.push(individual[field]);

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var putUri = (function()
    {
      return function(name, value)
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

    var setUri = function(name, value)
    {
      out_data0_el[name] = [
      {
        data: value,
        type: _Uri
      }];

      //if (typeof window === "undefined")
      //  print ("@1 out_data0_el=", toJson (out_data0_el));
      //print ("@1 out_data0_el[",name, "]=", toJson (out_data0_el[name]));
    }

    var putString = (function()
    {
      return function(name, value)
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

    var setString = (function()
    {
      return function(name, value)
      {
        var out_data0_el_arr;

        out_data0_el_arr = [];

        out_data0_el_arr.push(
        {
          data: value,
          type: _String
        });

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var setDatetime = (function()
    {
      return function(name, value)
      {
        var out_data0_el_arr;

        out_data0_el_arr = [];

        out_data0_el_arr.push(
        {
          data: value,
          type: _Datetime
        });

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var putDatetime = (function()
    {
      return function(name, value)
      {
        var out_data0_el_arr;

        out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
          out_data0_el_arr = [];

        out_data0_el_arr.push(
        {
          data: value,
          type: _Datetime
        });

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var putBoolean = (function()
    {
      return function(name, value)
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

    var setBoolean = (function()
    {
      return function(name, value)
      {
        var out_data0_el_arr;

        out_data0_el_arr = [];

        out_data0_el_arr.push(
        {
          data: value,
          type: _Bool
        });

        out_data0_el[name] = out_data0_el_arr;
      }
    })();


    var putInteger = (function()
    {
      return function(name, value)
      {
        var out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
          out_data0_el_arr = [];

        out_data0_el_arr.push(
        {
          data: value,
          type: _Integer
        });

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var setInteger = (function()
    {
      return function(name, value)
      {
        var out_data0_el_arr;

        out_data0_el_arr = [];

        out_data0_el_arr.push(
        {
          data: value,
          type: _Integer
        });

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var putExecutor = (function()
    {
      return function(name)
      {
        var out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
          out_data0_el_arr = [];

        if (Array.isArray(executor) === true)
        {
          for (var key3 in executor)
          {
            out_data0_el_arr.push(executor[key3]);
          }
        }
        else
          out_data0_el_arr.push(executor);

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var putWorkOrder = (function()
    {
      return function(name)
      {
        var out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
          out_data0_el_arr = [];

        if (Array.isArray(work_order) === true)
        {
          for (var key3 in work_order)
          {
            out_data0_el_arr.push(work_order[key3]);
          }
        }
        else
          out_data0_el_arr.push(work_order);

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var putThisProcess = (function()
    {
      return function(name)
      {
        var out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
          out_data0_el_arr = [];

        if (Array.isArray(process) === true)
        {
          for (var key3 in process)
          {
            out_data0_el_arr.push(process[key3]);
          }
        }
        else
          out_data0_el_arr.push(process);

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    var removeThisProcess = (function()
    {
      return function(name)
      {
        var out_data0_el_arr = out_data0_el[name];

        if (!out_data0_el_arr)
          out_data0_el_arr = [];

        if (Array.isArray(process) === true)
        {
          for (var key3 in process)
          {
            out_data0_el_arr = removeV (out_data0_el_arr, process[key3]);
          }
        }
        else
        {
          out_data0_el_arr = removeV (out_data0_el_arr, process);
        }

        out_data0_el[name] = out_data0_el_arr;
      }
    })();

    /* PUT functions [END] */

    for (var key in individuals)
    {
      //print("#1 key=", key);
      var individual = individuals[key];
      if (typeof window !== "undefined") individual['@'] = individual['id']; // sly hack

      //print("#1.1 key=", key);
      var objectContentStrValue = (function()
      {
        return function(name, value)
        {
          if (individual[name])
          {
            var result = false;
            if (typeof window === "undefined")
            {
              for (var i in individual[name])
              {
                if (value === individual[name][i].data)
                {
                  result = true;
                }
              }
            }
            else
            {
              for (var i in individual[name])
              {
                if (value === individual[name][i])
                {
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

      var iteratedObject = (typeof window === "undefined") ? Object.getOwnPropertyNames(individual) : Object.getOwnPropertyNames(individual.properties);

      for (var key2 = 0; key2 < iteratedObject.length; key2++)
      {
        var element = individual[iteratedObject[key2]];

        var putValue = (function()
        {
          return function(name)
          {
            var out_data0_el_arr = out_data0_el[name];

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
            {
              if (Array.isArray(element) === true)
              {
                for (var key3 in element)
                {
                  out_data0_el_arr.push(element[key3]);
                }
              }
              else
                out_data0_el_arr.push(element);
            }

            out_data0_el[name] = out_data0_el_arr;
          }
        })();

        var putValueFrom = (function()
        {
          return function(name, path, transform)
          {
            var out_data0_el_arr = out_data0_el[name];
            if (!out_data0_el_arr)
              out_data0_el_arr = [];

            var element_uri;

            if (Array.isArray(element) === true)
              element_uri = getUri (element);
            else
              element_uri = element.data ? element.data : element;

            var curelem;

            curelem = (typeof window === "undefined") ? get_individual(ticket, element_uri) : new veda.IndividualModel(element_uri);

            for (var i = 0; i < path.length - 1; i++)
            {
              if (!curelem || !curelem[path[i]]) return;

              if (typeof window === "undefined") {
                curelem = get_individual(ticket, curelem[path[i]].data ? curelem[path[i]].data : curelem[path[i]]);
              } else {
                curelem = curelem[path[i]][0];
              }
            }
            if (!curelem || !curelem[path[path.length - 1]]) return;

            if (typeof window === "undefined") {
                out_data0_el_arr.push(curelem[path[path.length - 1]]);
            } else {
              curelem[path[path.length - 1]].forEach(
                function(item) {
                  var value = item;
                  var valueType = _Uri;

                  if (value instanceof String) { valueType = _String; }
                  else if (value instanceof Date) { valueType = _Datetime; }
                  else if (value instanceof Number) { valueType = _Decimal; }
                  else if (value instanceof Boolean) { valueType = _Boolean; }
                  else value = value.id;

                  if (valueType == _Uri && typeof transform != 'undefined') {
                    if (transform == 'clone') {
                      value = value.clone();
                    } else {
                      value = veda.Util.buildStartFormByTransformation(value, new veda.IndividualModel(transform));
                    }
                  }

                  if (typeof value !== "undefined") {
                    out_data0_el_arr.push(
                    {
                      data: value,
                      type: valueType
                    });
                  }
                }
              );
            }

            out_data0_el[name] = out_data0_el_arr;
          }
        })();

        var putFrontValue = (function()
        {
          return function(name)
          {
            var out_data0_el_arr = out_data0_el[name];

            if (!out_data0_el_arr)
              out_data0_el_arr = [];
            if (iteratedObject[key2] == '@')
            {
              out_data0_el_arr.unshift(
              {
                data: element,
                type: _Uri
              });
            }
            else
            {
              if (Array.isArray(element) === true)
              {
                for (var key3 in element)
                {
                  out_data0_el_arr.unshift(element[key3]);
                }
              }
              else
                out_data0_el_arr.unshift(element);
            }

            out_data0_el[name] = out_data0_el_arr;
          }
        })();

        var putElement = (function()
        {
          return function()
          {
            var name = iteratedObject[key2];
            if (name == '@')
              return;

            var out_data0_el_arr = [];
            out_data0_el_arr = out_data0_el[name];

            if (!out_data0_el_arr)
              out_data0_el_arr = [];

            if (Array.isArray(element) === true)
            {
              for (var key3 in element)
              {
                out_data0_el_arr.push(element[key3]);
              }
            }
            else
              out_data0_el_arr.push(element);

            out_data0_el[name] = out_data0_el_arr;
          }
        })();

        /* Segregate functions [BEGIN] */
        var contentName = (function()
        {
          return function(name)
          {
            return iteratedObject[key2] == name;
          }
        })();

        var elementContentStrValue = (function()
        {
          return function(name, value)
          {
            if (iteratedObject[key2] !== name)
              return false;
            //print("individual[name]=", toJson(individual[name]));
            var str = typeof window === "undefined" ? element[0].data : element[0].id;
            //print("str=", str);
            if (str == value)
              return true;
            else
              return false;
          }
        })();
        /* Segregate functions [END] */

        var getElement = (function()
        {
          return function()
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
            if (typeof window === "undefined")
            {
              res = eval(segregateObject[0].data)
            }
            else if (segregateObject[0] != undefined)
            {
              res = eval(segregateObject[0].toString());
            }
            if (res == false)
              continue;
          }

          if (segregateElement)
          {
            if (typeof window === "undefined")
            {
              res = eval(segregateElement[0].data)
            }
            else if (segregateElement[0] != undefined)
            {
              res = eval(segregateElement[0].toString());
            }
            if (res == false)
              continue;
          }

          //print("#7 key=", key);
          //print("#7 element=", toJson(element));

          //if (segregateElement)
          //  print("#8 segregateElement=", segregateElement[0].data);

          // 3. v-wf:aggregate
          var group_key;
          if (!grouping)
          {
            out_data0_el = {};
            out_data0_el['@'] = genUri();
          }
          else
          {
            var useExistsUid = false;
            for (var i in grouping)
            {
              var gk = typeof window === "undefined" ? grouping[i].data : grouping[i];
              if (gk == '@')
                useExistsUid = true;
              else
                group_key = gk;
            }

            out_data0_el = out_data0[group_key];
            if (!out_data0_el)
            {
              out_data0_el = {};
              if (useExistsUid)
                out_data0_el['@'] = individual['@'];
              else
                out_data0_el['@'] = genUri();
            }
          }

          var agregate = rule['v-wf:aggregate'];
          for (var i2 = 0; i2 < agregate.length; i2++)
          {
            if (typeof window === "undefined")
            {
              eval(agregate[i2].data);
            }
            else if (agregate[i2] != undefined)
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

    //if (typeof window === "undefined")
    //  print("@E out_data0=", toJson (out_data0));

    var out_data = [];
    for (var key in out_data0)
    {
      out_data.push(out_data0[key]);
    }

    return out_data;
  }
  catch (e)
  {
    if (typeof window === "undefined")
    {
      print(e.stack);
    }
    else
    {
      console.log(e.stack);
    }
  }
}


/**
 * General function for getNextValue method for numerators
 *
 * @param ticket
 * @param scope - numerator scope
 * @param FIRST_VALUE - first value in scope
 * @returns
 */
function getNextValueSimple(ticket, scope, FIRST_VALUE)
{
  if (typeof scope == 'string')
  {
    try
    {
      scope = new veda.IndividualModel(scope, undefined, undefined, undefined, false);
    }
    catch (e)
    {
      return ''+FIRST_VALUE;
    }
  }
  if (typeof scope === 'undefined' || !scope['v-s:numerationCommitedInterval'] || scope['v-s:numerationCommitedInterval'].length == 0)
  {
    return ''+FIRST_VALUE;
  }
  var max = 0;

  if (typeof window === 'undefined')
  {
    scope['v-s:numerationCommitedInterval'].forEach(function(interval)
    {
      interval = new veda.IndividualModel(interval.id, undefined, undefined, undefined, false);
      if (interval['v-s:numerationCommitedIntervalEnd'][0].data > max)
      {
        max = interval['v-s:numerationCommitedIntervalEnd'][0].data;
      }
    });
  }
  else
  {
    scope['v-s:numerationCommitedInterval'].forEach(function(interval)
    {
      interval = new veda.IndividualModel(interval.id, undefined, undefined, undefined, false);
      if (interval['v-s:numerationCommitedIntervalEnd'][0] > max)
      {
        max = interval['v-s:numerationCommitedIntervalEnd'][0];
      }
    });
  }
  return ''+(max + 1);
}

function isNumerationValueAvailable(scope, value)
{
  if (typeof scope === 'string')
  {
    scope = new veda.IndividualModel(scope, undefined, undefined, undefined, false);
  }
  if (typeof window === 'undefined')
  {
    throw "not implemented";
  }
  else
  {
    if (typeof scope === 'undefined' || typeof scope['v-s:numerationCommitedInterval'] === 'undefined') return true;
    for (var i = 0; i < scope['v-s:numerationCommitedInterval'].length; i++)
    {
      var interval = new veda.IndividualModel(scope['v-s:numerationCommitedInterval'][i].id, undefined, undefined, undefined, false);
      if (interval['v-s:numerationCommitedIntervalBegin'][0] <= value && value <= interval['v-s:numerationCommitedIntervalEnd'][0])
      {
        return false;
        //max = interval['v-s:numerationCommitedIntervalEnd'][0];
      }
    }
    return true;
  }
}

/////////////// rights

function newUri(uri)
{
  return [{
    data: uri,
    type: _Uri
  }];
}

function newStr(_data, _lang)
{
  if (!_lang || _lang == 'NONE')
    _lang = 0;

  return [{
    data: _data,
    type: _String,
    lang: _lang
  }];
}

function newBool(_data)
{
  return [{
    data: _data,
    type: _Bool
  }];
}

function newInt(_data)
{
  return [{
    data: _data,
    type: _Integer
  }];
}

function newDecimal(_data)
{
  return [{
    data: _data,
    type: _Decimal
  }];
}

function newDate(_data)
{
  return [{
    data: _data,
    type: _Datetime
  }];
}

function addDay(_data, _days)
{
  if (!_data)
    _data = new Date();

  try
  {
    _data.setDate(_data.getDate() + _days);
  }
  catch (e)
  {
    print(e);
  }

  return _data;
}

function getStrings(field)
{
  var res = [];
  if (field)
  {
    for (var i in field)
    {
      res.push(field[i].data);
    }
  }
  return res;
}

function getUris(field)
{
  var res = [];
  if (field)
  {
    for (var i in field)
    {
      res.push(field[i].data);
    }
  }
  return res;
}

function getUri(field)
{
  if (field && field.length > 0)
  {
    return field[0].data;
  }
}

function isExists(field, value)
{
  if (field)
  {
    for (var i in field)
    {
      if (field[i].data == value.data && field[i].type == value.type)
        return true;
    }
  }
  return false;
}

function getFirstValue(field)
{
  if (field && field.length > 0)
  {
    if (field[0].type == _Integer)
    {
      return parseInt(field[0].data, 10);
    }
    else if (field[0].type == _Datetime)
      return new Date(field[0].data);

    return field[0].data;
  }
}

function getFirstValueUseLang(field, lang)
{
  for (var i in field)
  {
    if (field[i].lang == lang)
      return field[i].data;
  }
  return null;
}

//


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

function addToGroup(ticket, group, resource, rights, new_uri)
{
  if (new_uri)
  {
    var prev = get_individual(ticket, new_uri);
    if (prev)
    {
      //print ("JS: GROUP ALREADY EXISTS");
      return;
    }
  }

  if (!new_uri)
  new_uri = genUri();

  var new_membership_uri = genUri();
  var new_membership = {
    '@': new_membership_uri,
    'rdf:type': newUri('v-s:Membership'),
    'v-s:memberOf': newUri(group),
    'v-s:resource': newUri(resource)
  };

  if (rights) {
    for (var i = 0; i < rights.length; i++)
    {
      if (rights[i] == can_read)
        new_membership['v-s:canRead'] = newBool(true);
      else if (rights[i] == can_update)
        new_membership['v-s:canUpdate'] = newBool(true);
      else if (rights[i] == can_delete)
        new_membership['v-s:canDelete'] = newBool(true);
      else if (rights[i] == can_create)
        new_membership['v-s:canCreate'] = newBool(true);
      else if (rights[i] == cant_read)
        new_membership['v-s:canRead'] = newBool(false);
      else if (rights[i] == cant_update)
        new_membership['v-s:canUpdate'] = newBool(false);
      else if (rights[i] == cant_delete)
        new_membership['v-s:canDelete'] = newBool(false);
      else if (rights[i] == cant_create)
        new_membership['v-s:canCreate'] = newBool(false);
    }
  }

  var res = put_individual(ticket.id, new_membership);

  return [new_membership, res];
}

function removeFromGroup(ticket, group, resource)
{
  var new_membership_uri = genUri();
  var new_membership = {
    '@': new_membership_uri,
    'rdf:type': newUri('v-s:Membership'),
    'v-s:memberOf': newUri(group),
    'v-s:resource': newUri(resource),
    'v-s:deleted': newBool(true)
  };
  var res = put_individual(ticket.id, new_membership);

  return [new_membership, res];
}

function addRight(ticket, rights, subj_uri, obj_uri, new_uri)
{
    if (subj_uri == undefined || obj_uri == undefined) {
	var error = new Error();
	print("ERR! INVALID ARGS IN addRigth");
	print("subj_uri=", subj_uri);
	print("obj_uri=", obj_uri);
	print("Error stack:", error.stack);
    }
	

  if (new_uri)
  {
    var prev = get_individual(ticket, new_uri);
    if (prev)
    {
      //print ("JS: RIGHT ALREADY EXISTS");
      return;
    }
  }

  if (!new_uri)
    new_uri = genUri();

  var new_permission = {
    '@': new_uri,
    'rdf:type': newUri('v-s:PermissionStatement'),
    'v-s:permissionObject': newUri(obj_uri),
    'v-s:permissionSubject': newUri(subj_uri)
  };

  for (var i = 0; i < rights.length; i++)
  {
    if (rights[i] == can_read)
      new_permission['v-s:canRead'] = newBool(true);
    else if (rights[i] == can_update)
      new_permission['v-s:canUpdate'] = newBool(true);
    else if (rights[i] == can_delete)
      new_permission['v-s:canDelete'] = newBool(true);
    else if (rights[i] == can_create)
      new_permission['v-s:canCreate'] = newBool(true);
    else if (rights[i] == cant_read)
      new_permission['v-s:canRead'] = newBool(false);
    else if (rights[i] == cant_update)
      new_permission['v-s:canUpdate'] = newBool(false);
    else if (rights[i] == cant_delete)
      new_permission['v-s:canDelete'] = newBool(false);
    else if (rights[i] == cant_create)
      new_permission['v-s:canCreate'] = newBool(false);
  }

  var res = put_individual(ticket, new_permission, _event_id);

  //print("ADD RIGHT:", toJson(new_permission));
  return [new_permission, res];
}

function clone(obj)
{
  var copy;

  // Handle the 3 simple types, and null or undefined
  if (null == obj || "object" != typeof obj) return obj;

  // Handle Date
  if (obj instanceof Date) {
    copy = new Date();
    copy.setTime(obj.getTime());
    return copy;
  }

  // Handle Array
  if (obj instanceof Array) {
    copy = [];
    for (var i = 0, len = obj.length; i < len; i++) {
      copy[i] = clone(obj[i]);
    }
    return copy;
  }

  // Handle Object
  if (obj instanceof Object) {
    copy = {};
    for (var attr in obj) {
      if (obj.hasOwnProperty(attr)) copy[attr] = clone(obj[attr]);
    }
    return copy;
  }

  throw new Error("Unable to copy obj! Its type isn't supported.");
}
