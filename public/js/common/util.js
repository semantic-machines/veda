// Veda common utility functions
"use strict";

function hasValue(doc, prop, val)
{
  var any = !!(doc && doc[prop] && doc[prop].length);
  if (!val) return any;
  return !!(any && doc[prop].filter(function(i)
  {
      return (i.type === val.type && i.data === val.data);
  }).length);
}

function compare(a, b)
{
  if (typeof a === "function")
    return a.toString() === b.toString();
  else if (typeof a != "object" || typeof b != "object")
    return a === b;

  if (a instanceof Date)
  {
    return a.toString () == b.toString ();
    //return a.valueOf() == b.valueOf();
  }

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
        if (bb == "Uri")
            bb = 'Uri';
        else if (bb == "String")
            bb = 'String';
        else if (bb == "Integer")
            bb = 'Integer';
        else if (bb == "Datetime")
            bb = 'Datetime';
        else if (bb == "Decimal")
            bb = 'Decimal';
        else if (bb == "Boolean")
            bb = 'Boolean';
      }
      else if (taa == 'number' && tbb == 'string')
      {
        if (aa == "Uri")
          aa = 'Uri';
        else if (aa == "String")
          aa = 'String';
        else if (aa == "Integer")
          aa = 'Integer';
        else if (aa == "Datetime")
          aa = 'Datetime';
        else if (aa == "Decimal")
          aa = 'Decimal';
        else if (aa == "Boolean")
          aa = 'Boolean';
      }
    }
    else if(key == "lang")
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
    if (!result)
  return false;
  }
  return result;
}

function sleep(usec)
{
  var endtime = new Date().getTime() + usec;
  while (new Date().getTime() < endtime);
}

function get_property_chain(ticket, first, rest)
{
  var doc;
  doc = typeof first == "object" ? first : get_individual(ticket, first);

  //  print ('@js ------------------');
  //  print ('@js #1 doc=', veda.Util.toJson (doc));;

  var doc_first = doc;
  var field;

  for (var i = 1; i < arguments.length; i++)
  {
    field = doc[arguments[i]];
    if (field && (field[0].type == "Uri" || field[0].type == "Uri"))
    {
      doc = get_individual(ticket, field[0].data);
      //      print ('@js #2 doc=', veda.Util.toJson (doc));;
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

    if (!rules || !rules.length)
      return;

    //print ("@B start transform");
    var tmp_rules = [];
    //print ("rules_in=", veda.Util.toJson (rules));
    //print ("individuals=", veda.Util.toJson (individuals));
    for (var i in rules)
    {
      var rul = get_individual(ticket, rules[i].data);
      if (!rul)
      {
        print("not read rule [", veda.Util.toJson(rul), "]");
        continue;
      }
      else
        tmp_rules.push(rul);
    }
    rules = tmp_rules;

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
      };
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
      };
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
          type: "Uri"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
    })();

    var setUri = function(name, value)
    {
      out_data0_el[name] = [
      {
        data: value,
        type: "Uri"
      }];
    };

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
          type: "String"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "String"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "Datetime"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "Datetime"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "Boolean"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "Boolean"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "Integer"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
          type: "Integer"
        });

        out_data0_el[name] = out_data0_el_arr;
      };
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
      };
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
      };
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
      };
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
            out_data0_el_arr = out_data0_el_arr.filter(function (value) {return value.data !== process[key3];});
          }
        }
        else
        {
          out_data0_el_arr = out_data0_el_arr.filter(function (value) {return value.data !== process;});
        }

        out_data0_el[name] = out_data0_el_arr;
      };
    })();

    /* PUT functions [END] */

    for (var key in individuals)
    {
      //print("#1 key=", key);
      var individual = individuals[key];

      //print("#1.1 key=", key);
      var objectContentStrValue = (function()
      {
        return function(name, value)
        {
          if (individual[name])
          {
            var result = false;
            for (var i in individual[name])
            {
              if (value === individual[name][i].data)
              {
                result = true;
              }
            }
            return result;
          }
        };
      })();

      var iteratedObject = Object.keys(individual);

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
                type: "Uri"
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
          };
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

            curelem = get_individual(ticket, element_uri);

            for (var i = 0; i < path.length - 1; i++)
            {
              if (!curelem || !curelem[path[i]]) return;
              var uri = Array.isArray(curelem[path[i]]) && curelem[path[i]][0].data ? curelem[path[i]][0].data : curelem[path[i]];
              curelem = get_individual(ticket, uri);
            }
            if (!curelem || !curelem[path[path.length - 1]]) return;

            out_data0_el_arr = out_data0_el_arr.concat(curelem[path[path.length - 1]]);

            out_data0_el[name] = out_data0_el_arr;
          };
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
                type: "Uri"
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
          };
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
          };
        })();

        /* Segregate functions [BEGIN] */
        var contentName = (function()
        {
          return function(name)
          {
            return iteratedObject[key2] == name;
          };
        })();

        var elementContentStrValue = (function()
        {
          return function(name, value)
          {
            if (iteratedObject[key2] !== name)
              return false;
            var str = element[0].data;
            if (str == value)
              return true;
            else
              return false;
          };
        })();
        /* Segregate functions [END] */

        var getElement = (function()
        {
          return function()
          {
            return element;
          };
        })();


        // выполняем все rules
        for (var key3 in rules)
        {
          var rule = rules[key3];
          // 1. v-wf:segregateObject
          var segregateObject = rule['v-wf:segregateObject'];

          // 2. v-wf:segregateElement
          var segregateElement = rule['v-wf:segregateElement'];
          var grouping = rule['v-wf:grouping'];

          var res = undefined;

          if (segregateObject)
          {
            res = eval(segregateObject[0].data);
            if (res == false)
              continue;
          }

          if (segregateElement)
          {
            res = eval(segregateElement[0].data);
            if (res == false)
              continue;
          }

          // 3. v-wf:aggregate
          var group_key;
          if (!grouping)
          {
            out_data0_el = {};
            out_data0_el['@'] = veda.Util.genUri() + "-tr";
          }
          else
          {
            var useExistsUid = false;
            for (var i in grouping)
            {
              var gk = grouping[i].data;
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
                out_data0_el['@'] = veda.Util.genUri() + "-tr";
            }
          }

          var agregate = rule['v-wf:aggregate'];
          for (var i2 = 0; i2 < agregate.length; i2++)
          {
            eval(agregate[i2].data);
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
      if (typeof window === 'undefined') {
        scope = get_individual(ticket, scope);
      } else {
        scope = new veda.IndividualModel(scope, false);
      }
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
      interval = get_individual(ticket, interval.data);
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
      interval = new veda.IndividualModel(interval.id, false);
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
    scope = new veda.IndividualModel(scope, false);
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
      var interval = new veda.IndividualModel(scope['v-s:numerationCommitedInterval'][i].id, false);
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
    type: "Uri"
  }];
}

function newStr(_data, _lang)
{
  var value = {
    data: _data,
    type: "String"
  };
  if (_lang && _lang !== 'NONE')
    value.lang = _lang;
  return [ value ];
}

function newBool(_data)
{
  return [{
    data: _data,
    type: "Boolean"
  }];
}

function newInt(_data)
{
  return [{
    data: _data,
    type: "Integer"
  }];
}

function newDecimal(_data)
{
  return [{
    data: _data,
    type: "Decimal"
  }];
}

function newDate(_data)
{
  return [{
    data: _data,
    type: "Datetime"
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

function getData(field)
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
    if (field[0].type == "Integer")
    {
      return parseInt(field[0].data, 10);
    }
    else if (field[0].type == "Datetime")
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
  new_uri = veda.Util.genUri() + "-gr";

  var new_membership_uri = veda.Util.genUri() + "-mbh";
  var new_membership = {
    '@': new_membership_uri,
    'rdf:type': newUri('v-s:Membership'),
    'v-s:memberOf': newUri(group),
    'v-s:resource': newUri(resource)
  };

  if (rights) {
    for (var i = 0; i < rights.length; i++)
    {
      if (rights[i] === can_read) {
        new_membership['v-s:canRead'] = newBool(true);
      } else if (rights[i] === can_update) {
        new_membership['v-s:canUpdate'] = newBool(true);
      } else if (rights[i] === can_delete) {
        new_membership['v-s:canDelete'] = newBool(true);
      } else if (rights[i] === can_create) {
        new_membership['v-s:canCreate'] = newBool(true);
      } else if (rights[i] === cant_read) {
        new_membership['v-s:canRead'] = newBool(false);
      } else if (rights[i] === cant_update) {
        new_membership['v-s:canUpdate'] = newBool(false);
      } else if (rights[i] === cant_delete) {
        new_membership['v-s:canDelete'] = newBool(false);
      } else if (rights[i] === cant_create) {
        new_membership['v-s:canCreate'] = newBool(false);
      }
    }
  }

  var res = put_individual(ticket, new_membership, typeof _event_id !== "undefined" ? _event_id : undefined);

  return [new_membership, res];
}

function removeFromGroup(ticket, group, resource)
{
  var new_membership_uri = veda.Util.genUri() + "-mbh";
  var new_membership = {
    '@': new_membership_uri,
    'rdf:type': newUri('v-s:Membership'),
    'v-s:memberOf': newUri(group),
    'v-s:resource': newUri(resource),
    'v-s:deleted': newBool(true)
  };
  var res = put_individual(ticket, new_membership, typeof _event_id !== "undefined" ? _event_id : undefined);

  return [new_membership, res];
}

function addRight(ticket, rights, subj_uri, obj_uri, right_uri) {

  if (subj_uri === undefined || obj_uri === undefined) {
    var error = new Error();
    if (typeof window === "undefined") {
      print("ERR! addRight: INVALID ARGS IN");
      print("subj_uri=", subj_uri);
      print("obj_uri=", obj_uri);
      print("Error stack:", error.stack);
    } else {
      console.log("ERR! addRight: INVALID ARGS IN");
      console.log("subj_uri =", subj_uri);
      console.log("obj_uri =", obj_uri);
      console.log("Error stack:", error.stack);
    }
    return;
  }

  var uri = right_uri || veda.Util.genUri() + "-r";

  var permission = {
    '@': uri,
    'rdf:type': newUri('v-s:PermissionStatement'),
    'v-s:permissionObject': newUri(obj_uri),
    'v-s:permissionSubject': newUri(subj_uri)
  };

  for (var i = 0; i < rights.length; i++) {
    if (rights[i] === can_read) {
      permission['v-s:canRead'] = newBool(true);
    } else if (rights[i] === can_update) {
      permission['v-s:canUpdate'] = newBool(true);
    } else if (rights[i] === can_delete) {
      permission['v-s:canDelete'] = newBool(true);
    } else if (rights[i] === can_create) {
      permission['v-s:canCreate'] = newBool(true);
    } else if (rights[i] === cant_read) {
      permission['v-s:canRead'] = newBool(false);
    } else if (rights[i] === cant_update) {
      permission['v-s:canUpdate'] = newBool(false);
    } else if (rights[i] === cant_delete) {
      permission['v-s:canDelete'] = newBool(false);
    } else if (rights[i] === cant_create) {
      permission['v-s:canCreate'] = newBool(false);
    }
  }

  var res = put_individual(ticket, permission, typeof _event_id !== "undefined" ? _event_id : undefined);

  //print("ADD RIGHT:", veda.Util.toJson(permission));
  return [permission, res];
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

function complexLabel(individual) {

  individual = individual.properties || individual;
  var cache = {};
  cache[ individual["@"] ] = individual;
  function get (uri) {
    return cache[uri] ? cache[uri] : cache[uri] = get_individual(veda.ticket, uri);
  }

  //print("INDIVIDUAL =", JSON.stringify(individual));

  try {

    var availableLanguages = get("v-ui:AvailableLanguage");
    var languages = availableLanguages["rdf:value"].map(function (languageValue) {
      var languageUri = languageValue.data;
      var language = get(languageUri);
      return language["rdf:value"][0].data;
    });

    return individual["rdf:type"].reduce(function (acc, typeValue) {
      var typeUri = typeValue.data;
      var type = get(typeUri);
      if ( !type || !hasValue(type, "v-s:labelPattern") ) return;
      var pattern = type["v-s:labelPattern"][0].data;
      var result = languages.map(function (language) {
        var replaced = pattern.replace(/{(\s*([^{}]+)\s*)}/g, function (match, group) {
          var chain = group.split(".");
          if (chain[0] === "@") {
            chain[0] = individual["@"];
          }
          return get_localized_chain.apply({}, [language].concat(chain));
        });
        return {
          data: replaced,
          type: "String",
          lang: language
        };
      });
      return acc.concat(result);
    }, []);
  } catch (err) {
    //print(err, err.stack);
    return [];
  }

  function get_localized_chain(language, uri) {
    var properties = [].slice.call(arguments, 2);
    var intermediate = get(uri);
    if (!intermediate) { return ""; }
    for (var i = 0, property; (property = properties[i]); i++) {
      var length = properties.length;
      if (i === length - 1) {
        if (!intermediate[property] || !intermediate[property].length) return "";
        return intermediate[property].reduce(function (acc, value) {
          //print("VALUE =", JSON.stringify(value));
          if ( !value.lang || value.lang === "NONE" || value.lang.toLowerCase() === language.toLowerCase() ) {
            var data = value.data;
            if (data instanceof Date) {
              data = new Date(data.getTime() - (data.getTimezoneOffset() * 60000)).toISOString().substr(0, 10);
            }
            return acc += data;
          } else {
            return acc;
          }
        }, "");
      }
      if ( hasValue(intermediate, property) ) {
        var intermediateUri = intermediate[property][0].data;
        intermediate = get(intermediateUri);
        if (!intermediate) { return ""; }
      } else {
        return "";
      }
    }
    return "";
  }
}


// Veda common utility functions----------------------------------------

veda.Module(function Util(veda) { "use strict";

  veda.Util = veda.Util || {};

// ---------------------------------------------------------------------

  veda.Util.toJson = function (value) {
    return JSON.stringify(value, null, 2);
  }

  veda.Util.generate_passes = function (length, count) {
    var result = {};
    for (var i = 0; i < count; i++) {
      var pass = generate_pass(length);
      var hash = Sha256.hash(pass);
      result[pass] = hash;
    }
    return result;
  }

  function generate_pass (length) {
    var ranges = [[48, 57], [97, 122]];
    var pass = "";
    for (var i = 0; i < length; i++) {
      var range = ranges[randomInRange(0, ranges.length - 1)];
      var charcode = randomInRange(range[0], range[1]);
      pass += String.fromCharCode(charcode);
    }
    return pass;
  }

  function randomInRange(begin, end) {
    return Math.round(Math.random() * (end - begin) + begin);
  }

  veda.Util.simpleHash = function (str) {
    var hash = 0, char;
    if (str.length === 0) {
        return hash;
    }
    for (var i = 0; i < str.length; i++) {
      char = str.charCodeAt(i);
      hash = ((hash<<5)-hash)+char;
      hash = hash & hash;
    }
    return hash;
  };

  veda.Util.processQuery = function (q, sort, limit, delta, pause, fn) {
    if (typeof q === "object") {
      sort  = q.sort;
      limit = q.limit;
      delta = q.delta;
      pause = q.pause;
      fn    = q.fn;
      q     = q.query;
    }
    console.log((new Date()).toISOString(), "Process query results |||", "query:", q, " | ", "limit:", limit, " | ", "delta:", delta, " | ", "pause:", pause);
    var result = [], append = [].push, fetchingProgress = 0;
    console.time("Fetching total");
    fetchResult();
    return;

    function fetchResult(cursor) {
      var from = cursor || 0;
      query({
        ticket: veda.ticket,
        query: q,
        sort: sort || "'v-s:created' desc",
        from: from,
        top: delta,
        limit: limit,
        async: true
      }).then(function (query_result) {
        var cursor = query_result.cursor;
        var estimated = query_result.estimated;
        if ( limit > estimated ) {
          limit = estimated;
        }
        append.apply(result, query_result.result);
        if ( cursor/limit - fetchingProgress >= 0.05 ) {
          fetchingProgress = cursor/limit;
          console.log("Fetching progress:", Math.floor(fetchingProgress * 100) + "%", "(" + cursor, "of", limit + ")");
        }
        if ( cursor === estimated || cursor >= limit ) {
          console.log((new Date()).toString(), "Fetching done:", limit);
          console.timeEnd("Fetching total");
          result.splice(limit - cursor || limit); // cut result to limit
          veda.Util.processResult(result, delta, pause, fn);
        } else {
          fetchResult(query_result.cursor);
        }
      });
    }
  };

  veda.Util.processResult = function (result, delta, pause, fn) {
    var total = result.length;
    var processingProgress = 0;
    console.log((new Date()).toISOString(), "Process results |||", "total:", total, " | ", "delta:", delta, " | ", "pause:", pause);
    console.time("Processing total");
    processPortion();

    function processPortion() {
      var portion = result.splice(0, delta);
      portion.forEach( fn );
      if ( (total - result.length) / total - processingProgress >= 0.05 ) {
        processingProgress = (total - result.length) / total;
        console.log("Processing progress:", Math.floor(processingProgress * 100) + "%", "(" + (total - result.length), "of", total + ")");
      }
      if ( result.length ) {
        setTimeout ? setTimeout(processPortion, pause) : processPortion();
      } else {
        console.log("Processing done:", total);
        console.timeEnd("Processing total");
      }
    }
  };

  veda.Util.genUri = function () {
    var uid = veda.Util.guid(), re = /^\d/;
    return (re.test(uid) ? "d:a" + uid : "d:" + uid);
  };
  veda.Util.guid = function () {
    var d = new Date().getTime();
    if (typeof performance !== "undefined" && typeof performance.now === "function"){
      d += performance.now(); //use high-precision timer if available
    }
    return "xxxxxxxxxxxxxxxxxxxxxxxxxx".replace(/x/g, function (c) {
      var r = (d + Math.random() * 36) % 36 | 0;
      d = Math.floor(d / 36);
      return r.toString(36);
    });
  };

  veda.Util.isInteger = function (n) { return n % 1 === 0; }

  function zeroPref(n) {
    return n > 9 ? n : "0" + n;
  }

  veda.Util.formatValue = function (value) {
    var formatted;
    switch (true) {
      case value instanceof Date:
        formatted = formatDate(value);
        break;
      case value instanceof Number || typeof value === "number":
        formatted = formatNumber(value);
        break;
      default:
        formatted = typeof value !== "undefined" ? value.toString() : value;
    }
    return formatted;
  };
  function formatDate (date) {
    var day = date.getDate(),
      month = date.getMonth() + 1,
      year = date.getFullYear(),
      hours = date.getHours(),
      mins = date.getMinutes(),
      secs = date.getSeconds(),
      fdate, ftime;
    month = zeroPref(month); day = zeroPref(day);
    hours = zeroPref(hours); mins = zeroPref(mins); secs = zeroPref(secs);
    fdate = [day, month, year].join(".");
    ftime = [hours, mins, secs].join(":");
    return (fdate === "01.01.1970" ? "" : fdate) + (ftime === "00:00:00" ? "" : " " + ( secs === "00" ? ftime.substr(0, 5) : ftime) );
  };
  function formatNumber (n) {
    return (n+"").replace(/.(?=(?:[0-9]{3})+\b)/g, '$& ');
  };

  veda.Util.applyTransform = function (individualList, transform) {
    return transformation(veda.ticket, individualList, transform, null, null);
  };

  veda.Util.forSubIndividual = function (net, property, id, func) {
    if (net[property]===undefined) return;
    net[property].forEach(function(el) {
      if (el.id == id) {
        func(el);
      }
    });
  };

  veda.Util.removeSubIndividual = function (net, property, id) {
    if (net[property]===undefined) return undefined;
    return net[property].filter( function (item) {
      return item.id !== id;
    });
  };

  /*
   * from http://stackoverflow.com/questions/27266550/how-to-flatten-nested-array-in-javascript
   * by http://stackoverflow.com/users/2389720/aduch
   *
   * This is done in a linear time O(n) without recursion
   * memory complexity is O(1) or O(n) if mutable param is set to false
   */
  veda.Util.flatten = function (array, mutable) {
    var toString = Object.prototype.toString;
    var arrayTypeStr = '[object Array]';

    var result = [];
    var nodes = (mutable && array) || array.slice();
    var node;

    if (!array.length) {
      return result;
    }

    node = nodes.pop();

    do {
      if (toString.call(node) === arrayTypeStr) {
        nodes.push.apply(nodes, node);
      } else {
        result.push(node);
      }
    } while (nodes.length && (node = nodes.pop()) !== undefined);

    result.reverse(); // we reverse result to restore the original order
    return result;
  };

  veda.Util.unique = function (arr) {
    var n = {}, r = [];
    for(var i = 0; i < arr.length; i++) {
      if (!n[arr[i]]) {
        n[arr[i]] = true;
        r.push(arr[i]);
      }
    }
    return r;
  };

  veda.Util.queryFromIndividual = function (individual) {
    var query;
    var flat = flattenIndividual(individual.properties);
    if ( individual.hasValue("*") && individual.get("*")[0].indexOf("==") > 0 ) {
      return individual.get("*")[0];
    }
    var allProps = Object.getOwnPropertyNames(flat)
      .map(function (property_uri) {
        if (property_uri === "@" || property_uri === "v-s:isDraft") { return; }
        var values = flat[property_uri].sort(function compare(a, b) {
          return a.data < b.data ? - 1 : a.data === b.data ? 0 : 1;
        });
        var oneProp;
        switch (values[0].type) {
          case "Integer":
          case "Decimal":
            oneProp = "'" + property_uri + "'==[" + values[0].data + "," + values[values.length-1].data + "]";
            break;
          // Date
          case "Datetime":
            var start = new Date(values[0].data);
            var end = new Date(values[values.length-1].data);
            start.setHours(0,0,0,0);
            end.setHours(23,59,59,999);
            oneProp = "'" + property_uri + "'==[" + start.toISOString() + "," + end.toISOString() + "]";
            break;
          case "Boolean":
            oneProp = values
              .map( function (value) {
                return "'" + property_uri + "'=='" + value.data + "'";
              })
              .join(" || ");
            break;
          case "String":
            oneProp = values
              .filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                //return "'" + property_uri + "'=='" + value.data + "*'";
                var q = value.data;
                if ( !q.match(/[\+\-\*]/) ) {
                  q = q.split(" ")
                       .filter(function (token) { return token.length > 0; })
                       .map(function (token) { return "+" + token + "*"; })
                       .join(" ");
                }
                return "'" + property_uri + "'=='" + q + "'";
              })
              .join(" || ");
            break;
          case "Uri":
            oneProp = values
              .filter(function(item){return !!item && !!item.valueOf();})
              .map( function (value) {
                if (property_uri === "rdf:type") {
                  return "'" + property_uri + "'=='" + value.data + "'";
                } else {
                  return "'" + property_uri + "'=='" + value.data + "'";
                }
              })
              .join(" || ");
            break;
        }
        return oneProp ? "( " + oneProp + " )" : undefined;
      })
      .filter(function(item){return typeof item !== undefined;})
      .join(" && ");
    query = allProps ? "( " + allProps + " )" : undefined;
    return query;
  };

  function flattenIndividual(object, prefix, union, visited) {
    var uri = object["@"];
    union = typeof union !== "undefined" ? union : {};
    prefix = typeof prefix !== "undefined" ? prefix : "";
    visited = typeof visited !== "undefined" ? visited : [];
    if (visited.indexOf(uri) > -1) {
      return;
    } else {
      visited.push(uri);
    }
    for (var property_uri in object) {
      if (property_uri === "@") { continue; }
      var values = object[property_uri];
      var prefixed = prefix ? prefix + "." + property_uri : property_uri;
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        if (value.type === "Uri") {
          var individ = new veda.IndividualModel(value.data);
          if ( individ.isNew() ) {
            flattenIndividual(individ.properties, prefixed, union, visited);
          } else {
            union[prefixed] = union[prefixed] ? union[prefixed] : [];
            union[prefixed].push( value );
          }
        } else {
          union[prefixed] = union[prefixed] ? union[prefixed] : [];
          union[prefixed].push( value );
        }
      }
    }
    return union;
  }

  /**
   * @returns veda.IndividualModel - start form
   */
  veda.Util.buildStartFormByTransformation = function (individual, transform) {
    var transfromResult = veda.Util.applyTransform(individual.properties, transform.properties);
    var startForm = new veda.IndividualModel(transfromResult[0]);
    startForm.isNew(true);
    startForm.isSync(false);
    return startForm;
  };

});
