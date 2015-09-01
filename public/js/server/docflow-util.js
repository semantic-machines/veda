"use strict";

function create_work_item(ticket, process_uri, net_element_uri, parent_uri, _event_id)
{
    var new_uri = genUri();
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

    //print("[WORKFLOW]:create work item:" + new_uri);

    put_individual(ticket, new_work_item, _event_id);

	addRight(ticket, [can_read], "v-wf:WorkflowReadUser", new_uri); 

    return new_uri;
}


function Context(_src_data, _ticket)
{
    this.src_data = _src_data;
    this.ticket = _ticket;

    this.getExecutor = function ()
    {
        return this.src_data['v-wf:executor'];
    };

    this.get_results = function ()
    {
        return this.src_data;
    };

    this.if_all_executors_taken_decision = function (true_decision, false_decision)
    {
        //print("Context.src_data=", toJson(this.src_data));
        var count_agreed = 0;
        for (var i = 0; i < this.src_data.length; i++)
        {
            //	   print ("data[i].result=", data[i].result);
            if (this.src_data[i].result == true_decision)
            {
                count_agreed++;
            }
        }

        if (count_agreed == this.src_data.length)
        {
            return [
                {
                    'data': true_decision,
                    'type': _Uri
            }];
        }
        else
        {
            return [
                {
                    'data': false_decision,
                    'type': _Uri
            }];
        }
    };

    this.getLocalVariableValue = function (var_name)
    {
        //print ("src_data=", toJson (this.src_data));
        //print ("var_name=", var_name);
        return this.src_data[var_name];
    };

    this.getVariableValue = function (var_name)
    {
        return this.getVariableValueIO(var_name, 'v-wf:inVars');
    }

    this.getOutVariableValue = function (var_name)
    {
        //print ("CONTEXT::get out vars, var_name=", var_name);
        return this.getVariableValueIO(var_name, 'v-wf:outVars');
    }

    this.getVariableValueIO = function (var_name, io)
    {
        //        	print ("CONTEXT::getVariableValueIO src_data=" + toJson (this.src_data));
        var variables = this.src_data[io];

        if (variables)
        {
            for (var i = 0; i < variables.length; i++)
            {
                var variable = get_individual(this.ticket, variables[i].data);
                if (!variable) continue;
                //print ("CONTEXT::getVariableValueIO var=" + toJson (variable));

                var variable_name = getFirstValue(variable['v-wf:variableName']);

                //print("[WORKFLOW]:getVariableIO #0: work_item=" + this.src_data['@'] + ", var_name=" + variable_name + ", val=" + toJson(variable['v-wf:variableValue']));

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

    this.print_variables = function (io)
    {
        var variables = this.src_data[io];

        if (variables)
        {
            for (var i = 0; i < variables.length; i++)
            {
                var variable = get_individual(this.ticket, variables[i].data);
                if (!variable) continue;

                var variable_name = getFirstValue(variable['v-wf:variableName']);

                print("[WORKFLOW]:print_variable: work_item=" + this.src_data['@'] + ", var_name=" + variable_name + ", val=" + toJson(variable['v-wf:variableValue']));
            }

        }
    };

    this.get_result_value = function (field1, type1)
    {
        if (this.src_data && this.src_data.length > 0)
        {
            var rr = this.src_data[0][field1];
            if (rr)
                return [
                    {
                        'data': rr,
                        'type': type1
                }];
            else
                return null;
        }
    };
}

function generate_variable(ticket, def_variable, value, _process, _task, _local)
{
    var variable_name = getFirstValue(def_variable['v-wf:varDefineName']);

    //print("[WORKFLOW][generate_variable]: variable_define_name=" + variable_name);

    var new_uri = genUri();
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

    //print("[WORKFLOW][generate_variable]: new variable: " + toJson(new_variable));

    return new_variable;
}

function create_and_mapping_variables(ticket, mapping, _process, _task, _order, _local, f_store)
{
    //print("[WORKFLOW][create_and_mapping_variables]: mapping=" + toJson(mapping));
    var new_vars = [];
    if (!mapping) return [];

    var process;
    var task;
    var order;
    var local;

    if (_process)
        process = new Context(_process, ticket);

    if (_task)
        task = new Context(_task, ticket);

    if (_order)
        order = new Context(_order, ticket);

    if (_local)
        local = new Context(_local, ticket);

    for (var i = 0; i < mapping.length; i++)
    {
        var map = get_individual(ticket, mapping[i].data);
        //print("[WORKFLOW][create_and_mapping_variables]: map=" + toJson(map));
        var expression = getFirstValue(map['v-wf:mappingExpression']);
        if (!expression) continue;

        //print("[WORKFLOW][create_and_mapping_variables]: expression=" + expression);
        var res1 = eval(expression);
        //print("[WORKFLOW][create_and_mapping_variables]: res1=" + toJson(res1));
        if (!res1) continue;

        var mapToVariable_uri = getUri(map['v-wf:mapToVariable']);
        if (!mapToVariable_uri) continue;

        var def_variable = get_individual(ticket, mapToVariable_uri);
        if (!def_variable) continue;

        var new_variable = generate_variable(ticket, def_variable, res1, _process, _task, _local);
        if (new_variable)
        {
            if (f_store == true)
            {
                put_individual(ticket, new_variable, _event_id);

                new_vars.push(
                {
                    data: new_variable['@'],
                    type: _Uri
                });
   				addRight(ticket, [can_read], "v-wf:WorkflowReadUser", new_variable['@']);                    

            }
            else
            {
                new_vars.push(new_variable);
            }
        }
    }

    return new_vars;
}
//////////////////////////////////////////////////////////////////////////
function is_exists_result(data)
{
    for (var i = 0; i < data.length; i++)
    {
        if (data[i].result)
            return true;
    }

    return false;
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

function is_some_executor_taken_decision(data, decision)
{
    for (var i = 0; i < data.length; i++)
    {
        //	   print ("data[i].result=", data[i].result);
        if (data[i].result == decision)
        {
            return true;
        }
    }

    return false;
}

//////////////////////////////////////////////////////////////////////////

function find_in_work_item_tree(ticket, _process, compare_field, compare_value)
{
    var res = [];

    var f_workItemList = _process['v-wf:workItemList'];

    if (f_workItemList)
        rsffiwit(ticket, f_workItemList, compare_field, compare_value, res, _process);

    return res;
}

function rsffiwit(ticket, work_item_list, compare_field, compare_value, res, _parent)
{
    for (var idx = 0; idx < work_item_list.length; idx++)
    {
        var i_work_item = get_individual(ticket, work_item_list[idx].data);
        if (i_work_item)
        {
            var ov = i_work_item[compare_field];
            var isCompleted = i_work_item['v-wf:isCompleted'];

            if (ov && getUri(ov) == compare_value && !isCompleted)
                res.push(
                {
                    parent: _parent,
                    work_item: i_work_item
                });

            var f_workItemList = i_work_item['v-wf:workItemList'];

            if (f_workItemList)
                rsffiwit(ticket, f_workItemList, compare_field, compare_value, res, i_work_item);
        }

    }

}

///////////////////////////////////////////// JOURNAL //////////////////////////////////////////////////

function create_new_journal(ticket, process_uri, label)
{
    var new_journal_uri = getJournalUri(process_uri);
    var new_journal = {
        '@': new_journal_uri,
        'rdf:type': [
            {
                data: 'v-s:Journal',
                type: _Uri
          }],
        'v-wf:onProcess': [
            {
                data: process_uri,
                type: _Uri
          }]
    };

    if (label)
        new_journal['rdfs:label'] = label;

    put_individual(ticket, new_journal, _event_id);

}

function mapToJournal(map_container, ticket, _process, _task, _order)
{
    //print ("@mapToJournal.1");
    if (map_container)
    {
        //* выполнить маппинг для журнала 
        var journalVars = [];

        journalVars = create_and_mapping_variables(ticket, map_container, _process, _task, _order, null, false);
        if (journalVars)
        {
            var jornal_uri = getJournalUri(_process['@']);
            var new_journal_record = newJournalRecord(jornal_uri);

            for (var idx = 0; idx < journalVars.length; idx++)
            {
                var jvar = journalVars[idx];
                var name = getFirstValue(jvar['v-wf:variableName']);
                var value = jvar['v-wf:variableValue'];

                //print("@mapToJournal.2 name=" + name + ", value=" + toJson(value));
                new_journal_record[name] = value;
            }
            logToJournal(ticket, jornal_uri, new_journal_record);
        }
    }
}

function create_new_subprocess(ticket, f_useSubNet, f_executor, parent_net, f_inVars, document)
{
    var parent_process = document['@'];

    var use_net;

    if (f_useSubNet)
        use_net = f_useSubNet;
    else
        use_net = f_executor;

    print("[WORKFLOW][WO2.4] executor= " + getUri(f_executor) + " used net= " + getUri(use_net));

    //var ctx = new Context(work_item, ticket);
    //ctx.print_variables ('v-wf:inVars');
    var _started_net = get_individual(ticket, getUri(use_net));
    if (_started_net)
    {
        var new_process_uri = genUri();

        var new_process = {
            '@': new_process_uri,
            'rdf:type': [
                {
                    data: 'v-wf:Process',
                    type: _Uri
      }],
            'v-wf:instanceOf': use_net,
            'v-wf:parentWorkOrder': [
                {
                    data: parent_process,
                    type: _Uri
      }]
        };

        var msg = "экземпляр маршрута :" + getFirstValue(_started_net['rdfs:label']) + ", запущен из " + getFirstValue(parent_net['rdfs:label'])

        if (f_useSubNet)
            msg += ", для " + getUri(f_executor);

        new_process['rdfs:label'] = [
            {
                data: msg,
                type: _String
                  }];

        // возьмем входные переменные WorkItem	и добавим их процессу
        if (f_inVars)
            new_process['v-wf:inVars'] = f_inVars;

        if (f_useSubNet)
            new_process['v-wf:executor'] = f_executor;

        print("new_process=", toJson(new_process));
        put_individual(ticket, new_process, _event_id);

        create_new_journal(ticket, new_process_uri, _started_net['rdfs:label']);

        var journal_uri = getJournalUri(_process['@']);
        var new_journal_record = newJournalRecord(journal_uri);

        new_journal_record['rdf:type'] = [
            {
                data: 'v-wf:SubProcessStarted',
                type: _Uri
     }];
        new_journal_record['rdfs:label'] = [
            {
                data: 'запущен подпроцесс',
                type: _String
     }];
        new_journal_record['v-s:subJournal'] = [
            {
                data: getJournalUri(new_process_uri),
                type: _Uri
     }];
        logToJournal(ticket, journal_uri, new_journal_record);

        document['v-wf:isProcess'] = [
            {
                data: new_process_uri,
                type: _Uri
     }];
        put_individual(ticket, document, _event_id);

    }

}
