"use strict";

/*
 *   обработка формы решения пользователя
 */
function prepare_decision_form (ticket, document)
{	
	if (document['v-wf:isCompleted'])	
		return;
		
   	print("[WORKFLOW][DF1].0");
	var takenDecision = document['v-wf:takenDecision'];
	if (!takenDecision)
		return;
		
	print("[WORKFLOW][DF1] : ### ---------------------------- prepare_decision_form:" + document['@']);
	
	var onWorkOrder = document['v-wf:onWorkOrder'];
	var work_order = get_individual(ticket, getUri(onWorkOrder));
    if (!work_order) return;
    
   	print("[WORKFLOW][DF1].1");
    
    var forWorkItem_uri = getUri(work_order['v-wf:forWorkItem']);
    var work_item = get_individual(ticket, forWorkItem_uri);
    if (!work_item) return;

   	print("[WORKFLOW][DF1].2");
    
    var forNetElement = work_item['v-wf:forNetElement'];
    var netElement = get_individual(ticket, getUri(forNetElement));
    if (!netElement) return;

   	print("[WORKFLOW][DF1].3");
    
     var transform_link = getUri(netElement['v-wf:completeResultTransform']);
     if (!transform_link) return;
     var transform = get_individual(ticket, transform_link);
     if (!transform) return;

   	print("[WORKFLOW][DF1].4 document=", toJson (document));   	
   	print("[WORKFLOW][DF1].4 transform=", toJson (transform));
   	print("[WORKFLOW][DF1].4 work_order=", toJson (work_order));   	

    var process_output_vars = transformation(ticket, document, transform, null, onWorkOrder);

   	print("[WORKFLOW][DF1].5 transform_result=", toJson (process_output_vars));    
    var new_vars = [];
    for (var i = 0; i < process_output_vars.length; i++)
    {
        put_individual(ticket, process_output_vars[i], _event_id);
        new_vars.push(
        {
            data: process_output_vars[i]['@'],
            type: _Uri
        });
    }
    if (process_output_vars.length > 0) 
    {    
		work_order['v-wf:outputVariable'] = new_vars;   	
		put_individual(ticket, work_order, _event_id);
		
		document['v-wf:isCompleted'] = [
            {
                data: true,
                type: _Bool
               }];
               
		put_individual(ticket, document, _event_id);               
	}	
}


/*
 *   обработка рабочего задания
 */
function prepare_work_order(ticket, document)
{
    print("[WORKFLOW][WO1] : ### ---------------------------- prepare_work_order:" + document['@']);
    var executor_uri = document['v-wf:executor'];
    var executor = get_individual(ticket, getUri(executor_uri));
    if (!executor) return;

    var forWorkItem_uri = getUri(document['v-wf:forWorkItem']);
    var work_item = get_individual(ticket, forWorkItem_uri);
    if (!work_item) return;

    var work_item_inputVariable_uri = work_item['v-wf:inputVariable'];

    var forProcess_uri = getUri(work_item['v-wf:forProcess']);
    var process = get_individual(ticket, forProcess_uri);
    if (!process) return;

    var process_inputVariable_uri = process['v-wf:inputVariable'];

    var forNetElement = work_item['v-wf:forNetElement'];
    var netElement = get_individual(ticket, getUri(forNetElement));
    if (!netElement) return;

    var local_outputVariable = document['v-wf:outputVariable'];

	if (!local_outputVariable)
	{
    // если исполнитель коделет
    if (is_exist(executor, 'rdf:type', 'v-s:Codelet'))
    {
        print("[WORKFLOW][WO2] executor=" + getUri(executor_uri) + ", is codelet");

        var expression = getFirstValue(executor['v-s:script']);
        if (!expression) return;

        print("[WORKFLOW][WO3] expression=" + expression);

        var task = new Context(work_item, ticket);
        var result0 = eval(expression);

        print("[WORKFLOW][WO4] task: eval result=", toJson(result0));

        if (!netElement['v-wf:completedMapping'])
        {
            print("[WORKFLOW][WO W6] v-wf:completedMapping not defined=", toJson(netElement));
        }

        // сохраняем результаты в v-wf:outputVariable в обрабатываемом рабочем задании
        var task_output_vars = create_and_mapping_variables(ticket, netElement['v-wf:completedMapping'], process, work_item, result0);
        //print("[WORKFLOW][WO W6.1] task_output_vars=", toJson(task_output_vars));			

        if (task_output_vars.length > 0)
        {
            document['v-wf:outputVariable'] = task_output_vars;
            put_individual(ticket, document, _event_id);
        }

    } // end [is codelet]        
    else if (is_exist(executor, 'rdf:type', 'v-s:Appointment'))
    {
        print("[WORKFLOW][WO20] is USER, executor=" + getUri(executor_uri) + "");
        //print("work_item.inputVariable=", toJson(work_item_inputVariable_uri));
        //print("process.inputVariable=", toJson(process_inputVariable_uri));

        var work_item_inputVariable = [];
        for (var i = 0; i < work_item_inputVariable_uri.length; i++)
        {
            var indv = get_individual(ticket, work_item_inputVariable_uri[i].data);
            work_item_inputVariable.push(indv);
        }

        //print("[WORKFLOW][WO20.0] transform_link=" + toJson(netElement['v-wf:startResultTransform']));
        //print("[WORKFLOW][WO20.1] work_item_inputVariable=" + toJson(work_item_inputVariable));


        var transform_link = getUri(netElement['v-wf:startResultTransform']);
        if (!transform_link) return;
        var transform = get_individual(ticket, transform_link);
        if (!transform) return;

        var transform_result = transformation(ticket, work_item_inputVariable, transform, executor_uri, newUri (document['@']));
        
        for (var i = 0; i < transform_result.length; i++)
        {
			put_individual(ticket, transform_result[i], _event_id);
			// выдадим права отвечающему на эту форму
			var employee = executor['v-s:employee'];
			if (employee)
			{
				print("[WORKFLOW][WO20.3] employee=" + toJson(employee));
				
				addRight (ticket, [can_read, can_update], employee[0].data, transform_result[i]['@']);
			}	
		}	
        
        print("[WORKFLOW][WO20.2] transform_result=" + toJson(transform_result));
    }
    else
    {
        print("[WORKFLOW][WO21] executor=" + getUri(executor_uri) + "");
    }
	}
	
    var is_goto_to_next_task = false;

    // begin //////////////// скрипт сборки результатов
    var result = [];

    // найдем маппинг множественных результатов
    var wosResultsMapping = netElement['v-wf:wosResultsMapping'];

    var workOrderList = work_item['v-wf:workOrderList'];
    // проверяем есть ли результаты рабочих заданий
    for (var i = 0; i < workOrderList.length; i++)
    {
        //print("[WORKFLOW][WO30.0] workOrder=" + toJson (workOrderList[i]) + "");        
        var workOrder;
        if (workOrderList[i].data != document['@'])
            workOrder = get_individual(ticket, workOrderList[i].data);
        else
            workOrder = document;

        //print("[WORKFLOW][WO30.1] workOrder=" + toJson (workOrder) + "");        

        var outputVariable = workOrder['v-wf:outputVariable'];
        if (outputVariable)
        {
            var _result = get_individual(ticket, outputVariable[0].data);
            //print("[WORKFLOW][WO30.2] _result=" + toJson (_result) + "");        
            if (_result)
            {
                if (wosResultsMapping)
                {}
                else
                {
                    // wosResultsMapping не указан, складываем все результаты в локальную переменную
                    var el = {};
                    el['workOrder'] = workOrder['@'];
                    var key, val;
                    var varName = _result["v-wf:variableName"];
                    if (varName)
                        key = varName[0].data;

                    var varValue = _result["v-wf:variableValue"];
                    if (varValue)
                        val = varValue[0].data;

                    if (val !== undefined && key !== undefined)
                    {
                        el[key] = val;

                        result.push(el);
                    }
                    //print("[WORKFLOW][WO30.3] result=" + toJson (result) + "");        
                }
            }
        }

    }
    //print("[WORKFLOW][WO1-20]");

    if (result.length == workOrderList.length)
        is_goto_to_next_task = true;
    else
        print("[WORKFLOW][WO1-25] не все задания выполнены, stop.");

    // end //////////////// скрипт сборки результатов
    print("[WORKFLOW][WO30.e] result=" + toJson(result) + "");

    if (is_goto_to_next_task)
    {
        // определим переход на следующие задачи в зависимости от результата
        // res должен быть использован при eval каждого из предикатов
        var hasFlows = netElement['v-wf:hasFlow'];
        if (hasFlows)
        {
            var split = getUri(netElement['v-wf:split']);

            //if (split)
            //{

            for (var i = 0; i < hasFlows.length; i++)
            {
                var flow = get_individual(ticket, hasFlows[i].data);
                if (!flow) continue;

                print("[WORKFLOW][WO6]:Flow: " + flow['@']);

                var flowsInto = flow['v-wf:flowsInto'];
                if (!flowsInto) continue;

                var predicate = flow['v-wf:predicate'];
                if (predicate)
                {
                    //print("[WORKFLOW][WO7] eval res=" + toJson(res));

                    print("[WORKFLOW][WO8] predicate=" + toJson(predicate));
                    expression = getFirstValue(predicate);
                    print("[WORKFLOW][WO9] expression=" + toJson(expression));
                    print("[WORKFLOW][WO9.1] result=" + toJson(result));
                    if (expression)
                    {
                        var res1 = eval(expression);
                        //print("res1=" + res1);
                        if (res1 === true && split == 'v-wf:XOR')
                        {
                            // выполним переход по XOR условию								
                            var nextNetElement = get_individual(ticket, getUri(flowsInto));

                            if (nextNetElement)
                            {
                                print("[WORKFLOW][WO10] create next work item for =" + nextNetElement['@']);
                                create_work_item(ticket, forProcess_uri, nextNetElement['@'], work_item['@'], _event_id);
                            }

                        }
                    }
                }
                else
                {
                    // условия нет, выполним переход								
                    var nextNetElement = get_individual(ticket, getUri(flowsInto));

                    if (nextNetElement)
                    {
                        print("[WORKFLOW][WO11] create next work item for =" + nextNetElement['@']);
                        create_work_item(ticket, forProcess_uri, nextNetElement['@'], work_item['@'], _event_id);
                    }


                }

            }
            // }
        }

        work_item['v-wf:isCompleted'] = [
            {
                data: true,
                type: _Bool
               }];
        put_individual(ticket, work_item, _event_id);
        //print("[WORKFLOW][WO12] document=", toJson (document)); 
    }

}

/*
 *   обработка элемента сети
 * 
 * 			1. вычисление количества исполнителей, подготовка для них данных, запуск.
 * 			2. обработка результатов, ветвление 
 */
function prepare_work_item(ticket, document)
{
    print("[WORKFLOW]:prepare_work_item ### --------------------------------- " + document['@']);

    var isCompleted = document['v-wf:isCompleted'];

    if (isCompleted)
    {
        if (isCompleted[0].data === true)
        {
            print("[WORKFLOW]:prepare_work_item, completed, exit");
            return;
        }
    }

    var forProcess = getUri(document['v-wf:forProcess']);
    var process = get_individual(ticket, forProcess);
    if (!process) return;

    var instanceOf = getUri(process['v-wf:instanceOf']);
    var _net = get_individual(ticket, instanceOf);

    if (!_net) return;

    print("[WORKFLOW]:Process=" + process['@'] + ", net=" + _net['@']);

    var forNetElement = document['v-wf:forNetElement'];
    var netElement = get_individual(ticket, getUri(forNetElement));
    if (!netElement) return;

    print("\r\n[WORKFLOW]:-- NetElement:" + netElement['@'] + ' --');

    if (is_exist(netElement, 'rdf:type', 'v-wf:Task'))
    {
        print("[WORKFLOW]:Is task");

        // выполнить маппинг переменных	
        print("[WORKFLOW] task: start mapping vars");
        var task_input_vars = create_and_mapping_variables(ticket, netElement['v-wf:startingMapping'], process, document, null);
        if (task_input_vars.length > 0) document['v-wf:inputVariable'] = task_input_vars;
        print("task_input_vars=", toJson(task_input_vars));
        // сформировать список исполнителей
        var executor_list = [];
        var executor_uris = netElement['v-wf:executor'];

        if (executor_uris)
        {
            for (var i = 0; i < executor_uris.length; i++)
            {
                var executor = get_individual(ticket, executor_uris[i].data);

                if (is_exist(executor, 'rdf:type', 'v-wf:ExecutorDefinition'))
                {
                    // определение исполнителей посредством скрипта
                    print("[WORKFLOW] executor=" + executor_uris[i].data + ", script defined");

                    var expression = getFirstValue(executor['v-s:script']);
                    if (!expression) return;

                    print("[WORKFLOW] expression=" + expression);

                    var task = new Context(document, ticket);
                    //            var net = new Context(_net, ticket);
                    var process = new Context(process, ticket);
                    //var context = task;

                    var result = eval(expression);

                    print("[WORKFLOW] task: result of v-wf:ExecutorDefinition=", toJson(result));

                    if (result.length > 0)
                    {
                        for (var i3 = 0; i3 < result.length; i3++)
                        {
                            executor_list.push(result[i3]);
                        }
                    }
                }
                else
                {
                    executor_list.push(executor_uris[i]);
                }
            }
        }
        else
        {
            var subNet_uri = netElement['v-wf:subNet'];
            if (subNet_uri)
            {
                executor_list.push(subNet_uri[0]);
            }
        }

        print("[WORKFLOW] executor list =" + toJson(executor_list));

        var work_order_list = [];
        var work_order_uri_list = [];

        // сформировать задания для исполнителей
        for (var i = 0; i < executor_list.length; i++)
        {
            var new_work_order_uri = guid();

            var new_work_order = {
                '@': new_work_order_uri,
                'rdf:type': [
                    {
                        data: 'v-wf:WorkOrder',
                        type: _Uri
   }],
                'v-wf:forWorkItem': [
                    {
                        data: document['@'],
                        type: _Uri
   }],
                'v-wf:executor': executor_list[i]
            };

            work_order_list.push(new_work_order);
            work_order_uri_list.push(
            {
                data: new_work_order_uri,
                type: _Uri
            });
        }

        if (work_order_uri_list.length > 0)
            document['v-wf:workOrderList'] = work_order_uri_list;
        put_individual(ticket, document, _event_id);

        for (var i = 0; i < work_order_list.length; i++)
        {
            put_individual(ticket, work_order_list[i], _event_id);
        }

    } // end [Task]
    else if (is_exist(netElement, 'rdf:type', 'v-wf:InputCondition'))
    {
        print("[WORKFLOW]:Is input condition");
        var hasFlows = netElement['v-wf:hasFlow'];
        if (hasFlows)
        {
            for (var i = 0; i < hasFlows.length; i++)
            {
                var flow = get_individual(ticket, hasFlows[i].data);
                if (!flow) continue;

                print("[WORKFLOW]:Flow: " + flow['@']);

                var flowsInto = flow['v-wf:flowsInto'];
                if (!flowsInto) continue;

                var nextNetElement = get_individual(ticket, getUri(flowsInto));

                if (!nextNetElement) continue;

                create_work_item(ticket, forProcess, nextNetElement['@'], document['@'], _event_id);

                document['v-wf:isCompleted'] = [
                    {
                        data: true,
                        type: _Bool
               }];
                put_individual(ticket, document, _event_id);
                print("[WORKFLOW][WO12] document=", toJson(document));

            }
        }
    } // end InputCondition
    else if (is_exist(netElement, 'rdf:type', 'v-wf:Condition'))
    {
        print("[WORKFLOW]:Is condition");
        var hasFlows = netElement['v-wf:hasFlow'];
        if (hasFlows)
        {
            for (var i = 0; i < hasFlows.length; i++)
            {
                var flow = get_individual(ticket, hasFlows[i].data);
                if (!flow) continue;

                print("[WORKFLOW]:Flow: " + flow['@']);

                var flowsInto = flow['v-wf:flowsInto'];
                if (!flowsInto) continue;

                var nextNetElement = get_individual(ticket, getUri(flowsInto));

                if (!nextNetElement) continue;

                create_work_item(ticket, forProcess, nextNetElement['@'], document['@'], _event_id);

                document['v-wf:isCompleted'] = [
                    {
                        data: true,
                        type: _Bool
               }];
                put_individual(ticket, document, _event_id);
                print("[WORKFLOW][WO12] document=", toJson(document));

            }
        }
    }

}

/*
    обработка процесса
*/
function prepare_process(ticket, document)
{
    print("[WORKFLOW]:### prepare_process:" + document['@']);

    //		var forNet = document['v-wf:instanceOf'];
    //		var net = get_individual (ticket, getUri (forNet));
    //		if (!net)
    //		return;
}

/*
    Обработка стартовой формы и создание экземпляра процесса.
    Условие запуска процесса: в стартовой форме не должно быть поля v-wf:isProcess.
    создается экземпляр v-wf:Process с заполненными переменными из текущей формы
    и экземпляр v-wf:WorkItem относящийся к v-wf:InputCondition
*/
function prepare_start_form(ticket, document)
{
    print("[WORKFLOW]:prepare_start_form #B, doc_id=" + document['@']);

	if (document['v-wf:isProcess'])
	{		
		print("[WORKFLOW]:prepare_start_form, already started.");
		return;
	}	
		
    var new_process_uri = guid();

    var forNet = document['v-wf:forNet'];
    var net = get_individual(ticket, getUri(forNet));
    if (!net) return;

    //    var new_vars = [];

    var new_process = {
        '@': new_process_uri,
        'rdf:type': [
            {
                data: 'v-wf:Process',
                type: _Uri
        }],
        'v-wf:instanceOf': forNet
    };

    var transform_link = getUri(document['v-wf:useTransformation']);
    if (!transform_link) return;

    var transform = get_individual(ticket, transform_link);
    if (!transform) return;

    // формируем входящие переменные
    var process_input_vars = transformation(ticket, document, transform, null, null);
    var new_vars = [];
    for (var i = 0; i < process_input_vars.length; i++)
    {
        put_individual(ticket, process_input_vars[i], _event_id);
        new_vars.push(
        {
            data: process_input_vars[i]['@'],
            type: _Uri
        });
    }
    if (process_input_vars.length > 0) new_process['v-wf:inputVariable'] = new_vars;

    print("new_process=", toJson(new_process));

    /*
        // формируем локальные переменные	
        var process_local_vars = [];
        var net_local_variable = net['v-wf:localVariable'];
        if (net_local_variable)
        {
            for (var i = 0; i < net_local_variable.length; i++)
            {
                var net_variable = get_individual(ticket, net_local_variable[i].data);
                if (!net_variable) continue;

                var variable_name = getFirstValue(net_variable['v-wf:variableName']);

                var new_uri = guid();
                var new_process_variable = {
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
                put_individual(ticket, new_process_variable, _event_id);

                process_local_vars.push(
                {
                    data: new_uri,
                    type: _Uri
                });
            }
            if (process_local_vars.length > 0) new_process['v-wf:localVariable'] = process_local_vars;
        }
    */
    put_individual(ticket, new_process, _event_id);

    document['v-wf:isProcess'] = [
        {
            data: new_process_uri,
            type: _Uri
    }];
    put_individual(ticket, document, _event_id);
    print("[WORKFLOW]:new_process:" + new_process['@']);

    var net_consistsOfz = net['v-wf:consistsOf'];
    if (net_consistsOfz)
    {
        for (var i = 0; i < net_consistsOfz.length; i++)
        {
            var net_consistsOf = get_individual(ticket, net_consistsOfz[i].data);
            if (!net_consistsOf) continue;

            if (is_exist(net_consistsOf, 'rdf:type', 'v-wf:InputCondition'))
            {
                create_work_item(ticket, new_process_uri, net_consistsOf['@'], null, _event_id);
                break;
            }
        }
    }

    print("[WORKFLOW]:### prepare_start_form #E");
}

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
        var variables = this.src_data['v-wf:inputVariable'];

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

        var mapToVariable = getUri(map['v-wf:mapToVariable']);
        if (!mapToVariable) continue;

        var dest_variable = get_individual(ticket, mapToVariable);
        if (!dest_variable) continue;

        var variable_name = getFirstValue(dest_variable['v-wf:variableName']);
        print("[WORKFLOW][create_and_mapping_variables]: variable_name=" + variable_name);

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
            }],
            'v-wf:variableValue': res1
        };

        print("[WORKFLOW][create_and_mapping_variables]: new variable: " + toJson(new_variable));

        put_individual(ticket, new_variable, _event_id);

        new_vars.push(
        {
            data: new_uri,
            type: _Uri
        });
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

function down_right_and_store(process, task)
{
    var doc_id = task.getVariableValue('docId');

    if (doc_id)
    {
        var instanceOf = getUri(process['v-wf:instanceOf']);

        var net_doc_id = instanceOf + "_" + doc_id[0].data;
        print("[WORKFLOW]:down_right_and_store, find=", net_doc_id);

    }
    return {
        'right': [
            {
                data: 'acl1',
                type: _String
                }]
    };
}

function restore_right(process, task)
{
    print("[WORKFLOW]:restore_right function RESTORE RIGHT IS NOT IMPLIMENTED");
    var right = process.getVariableValue('right');
    print("[WORKFLOW]:restore_right ", toJson(right));
}

function is_in_docflow_and_set_if_true(process, task)
{
    var res = false;

    if (task)
    {
        var doc_id = task.getVariableValue('docId');
        if (doc_id)
        {
            var instanceOf = getUri(process['v-wf:instanceOf']);

            var net_doc_id = instanceOf + "_" + doc_id[0].data;
            print("[WORKFLOW]:is_in_docflow_and_set_if_true, find=", net_doc_id);

            var in_doc_flow = get_individual(process.ticket, net_doc_id);

            if (in_doc_flow)
            {
                res = true;
            }
            else
            {
                var new_doc = {
                    '@': net_doc_id,
                    'rdf:type': [
                        {
                            data: 'v-wf:Variable',
                            type: _Uri
                }]
                };
                put_individual(process.ticket, new_doc, _event_id);
            }

        }

    }

    var res_out = {
        'res':
        {
            data: res,
            type: _Bool
        }
    };

    return res_out;
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

        var contentStrValue = (function ()
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

                var putElement = (function ()
                {
                    return function (name)
                    {
                        out_data0_el[name] = element;
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
						
						out_data0_el_arr.push ({
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
                        out_data0_el[name] = [
                            {
                                data: value,
                                type: _String
                        }];
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
