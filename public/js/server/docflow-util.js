"use strict";

function create_work_item(ticket, process_uri, net_element_uri, parent_uri, _event_id)
{
    var new_uri = guid();
    var new_work_item = {
        '@': new_uri,
        'rdf:type': [
            {
                data: 'v-wf:WorkItem',
                type: _Uri
        }],
        'v-wf:forProcess': [
            {
                data: process_uri,
                type: _Uri
        }],
        'v-wf:forNetElement': [
            {
                data: net_element_uri,
                type: _Uri
        }]
    };

    if (parent_uri !== null)
    {
        new_work_item['v-wf:previousWorkItem'] = [
            {
                data: parent_uri,
                type: _Uri
   }];
    }

    print("[WORKFLOW]:create work item:" + new_uri);

    put_individual(ticket, new_work_item, _event_id);

    return new_uri;
}


function Context(_src_data, _ticket)
{
    this.src_data = _src_data;
    this.ticket = _ticket;

    this.getLocalVariableValue = function (var_name)
    {
        //print ("src_data=", toJson (this.src_data));
        //print ("var_name=", var_name);
        return this.src_data[var_name];
    };

    this.getVariableValue = function (var_name)
    {
        //	print ("src_data=" + toJson (this.src_data));
        var variables = this.src_data['v-wf:inVars'];

        if (variables)
        {
            for (var i = 0; i < variables.length; i++)
            {
                var variable = get_individual(this.ticket, variables[i].data);
                if (!variable) continue;

                var variable_name = getFirstValue(variable['v-wf:variableName']);

                //print("[WORKFLOW]:getVariableValue #0: work_item=" + this.src_data['@'] + ", var_name=" + variable_name + ", val=" + toJson(variable['v-wf:variableValue']));

                if (variable_name == var_name)
                {
                    var val = variable['v-wf:variableValue'];

                    //print("[WORKFLOW]:getVariableValue #1: work_item=" + this.src_data['@'] + ", var_name=" + var_name + ", val=" + toJson(val)); // + ", variable=" + toJson (variable));
                    return val;
                }
            }

        }
        //print("[WORKFLOW]:getVariableValue: work_item=" + this.src_data['@'] + ", var_name=" + var_name + ", val=undefined");
    };
}

function generate_variable(ticket, def_variable, value, _process, _task, _local)
{
    var variable_name = getFirstValue(def_variable['v-wf:varDefineName']);

    print("[WORKFLOW][generate_variable]: variable_define_name=" + variable_name);

    var new_uri = guid();
    var new_variable = {
        '@': new_uri,
        'rdf:type': [
            {
                data: 'v-wf:Variable',
                type: _Uri
            }],
        'v-wf:variableName': [
            {
                data: variable_name,
                type: _String
            }]
    };

    if (value)
        new_variable['v-wf:variableValue'] = value;

    var variable_scope = getUri(def_variable['v-wf:varDefineScope']);
    if (variable_scope)
    {
        var scope;
        if (variable_scope == 'v-wf:Net')
            scope = _process['@'];

        if (scope)
        {
            // найдем среди локальных переменных процесса, такую переменную
            // если нашли, то новая переменная должна перезаписать переменную процесса
            var local_vars = _process['v-wf:inVars'];
            if (local_vars)
            {
                var find_local_var;
                for (var i = 0; i < local_vars.length; i++)
                {
                    var local_var = get_individual(ticket, local_vars[i].data);
                    if (!local_var) continue;

                    var var_name = getFirstValue(local_var['v-wf:variableName']);
                    if (!var_name) continue;

                    if (var_name == variable_name)
                    {
                        find_local_var = local_var;
                        break;
                    }
                }

                if (find_local_var)
                    new_variable['@'] = find_local_var['@'];
            }

            new_variable['v-wf:variableScope'] = [
                {
                    data: scope,
                    type: _Uri
    }];
        }
    }

    print("[WORKFLOW][generate_variable]: new variable: " + toJson(new_variable));

    return new_variable;
}

function create_and_mapping_variables(ticket, mapping, _process, _task, _local)
{
    //print("[WORKFLOW][create_and_mapping_variables]: data=" + toJson (result));
    print("[WORKFLOW][create_and_mapping_variables]: mapping=" + toJson(mapping));
    var new_vars = [];
    if (!mapping) return [];

    var process;
    var task;
    var local;

    if (_process)
        process = new Context(_process, ticket);

    if (_task)
        task = new Context(_task, ticket);

    if (_local)
        local = new Context(_local, ticket);

    for (var i = 0; i < mapping.length; i++)
    {
        var map = get_individual(ticket, mapping[i].data);
        print("[WORKFLOW][create_and_mapping_variables]: map=" + toJson(map));
        var expression = getFirstValue(map['v-wf:mappingExpression']);
        if (!expression) continue;

        print("[WORKFLOW][create_and_mapping_variables]: expression=" + expression);
        var res1 = eval(expression);
        print("[WORKFLOW][create_and_mapping_variables]: res1=" + toJson(res1));
        if (!res1) continue;

        var mapToVariable_uri = getUri(map['v-wf:mapToVariable']);
        if (!mapToVariable_uri) continue;

        var def_variable = get_individual(ticket, mapToVariable_uri);
        if (!def_variable) continue;

        var new_variable = generate_variable(ticket, def_variable, res1, _process, _task, _local);
        if (new_variable)
        {
            put_individual(ticket, new_variable, _event_id);

            new_vars.push(
            {
                data: new_variable['@'],
                type: _Uri
            });
        }
    }

    return new_vars;
}

function is_all_executors_taken_decision(data, decision)
{
    var count_agreed = 0;
    for (var i = 0; i < data.length; i++)
    {
        //	   print ("data[i].result=", data[i].result);
        if (data[i].result == decision)
        {
            count_agreed++;
        }
    }

    if (count_agreed == data.length)
    {
        return true;
    }
    else
    {
        return false;
    }
}


function transformation(ticket, _in_data, rule, executor, work_order)
{
    var in_data = [];
    var out_data0 = {};

    if (Array.isArray(_in_data) === true)
        in_data = _in_data;
    else
        in_data.push(_in_data);

    var transformRule = rule['v-wf:transformRule'];

    if (!transformRule)
        return;

    var rules = [];

    for (var i = 0; i < transformRule.length; i++)
    {
        var rr = get_individual(ticket, transformRule[i].data);
        if (rr)
            rules.push(rr);
    }

    //print("#6 in_data=", toJson(in_data));
    var out_data0_el = {};

    for (var i = 0; i < in_data.length; i++)
    {
        var obj = in_data[i];

        var objectContentStrValue = (function ()
        {
            return function (name, value)
            {
                //print("obj[name]=", toJson(obj[name]));
                var str = obj[name][0].data;
                //print("str=", str);
                if (str == value)
                    return true;
                else
                    return false;
            }
        })();



        for (var key in obj)
        {
            var element = obj[key];

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
            for (var i1 = 0; i1 < rules.length; i1++)
            {
                // 1. v-wf:segregateObject
                var segregateObject = rules[i1]['v-wf:segregateObject'];

                // 2. v-wf:segregateElement
                var segregateElement = rules[i1]['v-wf:segregateElement'];
                var grouping = rules[i1]['v-wf:grouping'];

                var res;

                if (segregateObject)
                {
                    res = eval(segregateObject[0].data);
                    if (res == false)
                        continue;
                }

                var elementContentStrValue = (function ()
                {
                    return function (name, value)
                    {
                        if (key !== name)
                            return false;
                        //print("obj[name]=", toJson(obj[name]));
                        var str = element[0].data;
                        //print("str=", str);
                        if (str == value)
                            return true;
                        else
                            return false;
                    }
                })();

                res = eval(segregateElement[0].data);
                if (res == false)
                    continue;

                var getElement = (function ()
                {
                    return function ()
                    {
                        return element;
                    }
                })();

                var putTypeOfElement = (function ()
                {
                    return function (name)
                    {
                        var rr = get_individual(ticket, getUri(element));
                        if (!rr)
                            return;

                        var out_data0_el_arr;

                        out_data0_el_arr = out_data0_el[name];

                        if (!out_data0_el_arr)
                            out_data0_el_arr = [];

                        out_data0_el_arr.push(rr['rdf:type']);

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

                        out_data0_el_arr.push(element);

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

                // 3. v-wf:agregate
                var group_key;
                if (!grouping)
                {
                    out_data0_el = {};
                    out_data0_el['@'] = guid();
                }
                else
                {
                    group_key = grouping[0].data;
                    out_data0_el = out_data0[group_key];
                    if (!out_data0_el)
                    {
                        out_data0_el = {};
                        out_data0_el['@'] = guid();
                    }
                }

                var agregate = rules[i1]['v-wf:agregate'];
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
        //print("out_data0[", key, "]=", toJson(out_data0[key]));
        out_data.push(out_data0[key]);
    }

    return out_data;
}
