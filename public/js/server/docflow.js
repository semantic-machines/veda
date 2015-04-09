"use strict";

/*
*   обработка рабочего задания
*/
function prepare_work_order(ticket, document)
{
    print("[WORKFLOW][WO1] : ### ---------------------------- prepare_work_order:" + document['@']);
    var executor_uri = getUri(document['v-wf:executor']);
	var executor = get_individual(ticket, executor_uri);   
    if (!executor) return;
	
	var forWorkItem_uri = getUri(document['v-wf:forWorkItem']);
	var work_item = get_individual(ticket, forWorkItem_uri);
    if (!work_item) return;
	
	var forProcess_uri = getUri(work_item['v-wf:forProcess']);
    var process = get_individual(ticket, forProcess_uri);
    if (!process) return;
    	
    var forNetElement = work_item['v-wf:forNetElement'];
    var netElement = get_individual(ticket, getUri(forNetElement));
    if (!netElement) return;
    	
    var workOrderList = work_item['v-wf:workOrderList'];	
    	
    // если исполнитель коделет
    if (is_exist(executor, 'rdf:type', 'v-s:Codelet'))
    {
		print("[WORKFLOW][WO2] executor=" + executor_uri + ", is codelet");

        var expression = getFirstValue(executor['v-s:script']);
        if (!expression) return;

        print("[WORKFLOW][WO3] expression=" + expression);


        var task = new Context(work_item, ticket);
        var result = eval(expression);

        print("[WORKFLOW][WO4] task: eval result=", toJson(result));
        
        if (!netElement['v-wf:completedMapping'])
        {
			print("[WORKFLOW][WO W6] v-wf:completedMapping not defined=", toJson(netElement));			
		}	
        
        // сохраняем результаты
        var task_output_vars = create_and_mapping_variables(ticket, netElement['v-wf:completedMapping'], process, work_item, result);
        if (task_output_vars.length > 0)
        {
			document['v-wf:outputVariable'] = task_output_vars;						
			put_individual(ticket, document, _event_id);	
		}
			
     } // end [is codelet]        
     else
     {
		print("[WORKFLOW][WO20] executor=" + executor_uri + "");
			
	 }
						
        
        // скрипт сборки результатов
        
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
                    print("[WORKFLOW][WO7] eval res=" + toJson(res));

                    print("[WORKFLOW][WO8] predicate=" + toJson(predicate));
                    expression = getFirstValue(predicate);
                    print("[WORKFLOW][WO9] expression=" + toJson(expression));
                    if (expression)
                    {
						var res1 = eval(expression);
                        //print("res1=" + res1);
                        if (res1 == true && split == 'v-wf:XOR')
                        {
                            // выполним переход по XOR условию								
                            var nextNetElement = get_individual(ticket, getUri(flowsInto));

                            if (nextNetElement)
                            {
                                print("[WORKFLOW][WO10] create next work item for =" + nextNetElement['@']);
                                create_work_item(ticket, forProcess_uri, nextNetElement['@'], _event_id);
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
                        create_work_item(ticket, forProcess_uri, nextNetElement['@'], _event_id);
                    }
                 }

            }
                // }
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
    print("[WORKFLOW]: ### ---------------------------------  prepare_work_item:" + document['@']);

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
							executor_list.push (result[i3]);
						}	
					}
				}
				else
				{
					executor_list.push (executor_uris[i]);
				}	
			}
		}	
		else
		{
            var subNet_uri = netElement['v-wf:subNet'];
            if (subNet_uri)
            {
				executor_list.push (subNet_uri[0]);
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
			
			work_order_list.push (new_work_order);
			work_order_uri_list.push (
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

                create_work_item(ticket, forProcess, nextNetElement['@'], _event_id);
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

                create_work_item(ticket, forProcess, nextNetElement['@'], _event_id);
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
    print("[WORKFLOW]:### prepare_start_form #B _event_id=" + _event_id);

    var new_process_uri = guid();

    var decomposition_link = getUri(document['v-wf:useDecomposition']);
    if (!decomposition_link) return;

    var decomposition = get_individual(ticket, decomposition_link);
    if (!decomposition) return;

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

    // формируем входящие переменные
    var process_input_vars = create_and_mapping_variables(ticket, decomposition['v-wf:startingMapping'], new_process, null, null);
    if (process_input_vars.length > 0) new_process['v-wf:inputVariable'] = process_input_vars;

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
                create_work_item(ticket, new_process_uri, net_consistsOf['@'], _event_id);
                break;
            }
        }
    }

    print("[WORKFLOW]:### prepare_start_form #E");
}

function create_work_item(ticket, process_uri, net_element_uri, _event_id)
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

    print("[WORKFLOW]:create work item:" + new_uri);

    put_individual(ticket, new_work_item, _event_id);
}

function Context(_src_data, _ticket)
{
    this.src_data = _src_data;
    this.ticket = _ticket;

    this.getLocalVariableValue = function (var_name)
    {
		return this.src_data[var_name];
	}	

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
    }
}

function create_and_mapping_variables(ticket, mapping, _process, _task, _local)
{
//    print("[WORKFLOW][create_and_mapping_variables]: data=" + toJson (result));
//    print("[WORKFLOW][create_and_mapping_variables]: mapping=" + toJson (mapping));
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
//        print("[WORKFLOW][create_and_mapping_variables]: map=" + toJson (map));
        var expression = getFirstValue(map['v-wf:mappingExpression']);
        if (!expression) continue;

//        print("[WORKFLOW][create_and_mapping_variables]: expression=" + expression);
        var res1 = eval(expression);
//        print("[WORKFLOW][create_and_mapping_variables]: res1=" + toJson (res1));
        if (!res1) continue;

        var mapsTo = getUri(map['v-wf:mapsTo']);
        if (!mapsTo) continue;

        var dest_variable = get_individual(ticket, mapsTo);
        if (!dest_variable) continue;

        var variable_name = getFirstValue(dest_variable['v-wf:variableName']);
//        print("[WORKFLOW][create_and_mapping_variables]: variable_name=" + variable_name);

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

//        print("[WORKFLOW][create_and_mapping_variables]: new variable: " + toJson(new_variable));

        put_individual(ticket, new_variable, _event_id);

        new_vars.push(
        {
            data: new_uri,
            type: _Uri
        });
    }

    return new_vars;
}


function down_right_and_store(process, task)
{
    var doc_id = task.getVariableValue('docId');

    if (doc_id)
    {
        var instanceOf = getUri(process['v-wf:instanceOf']);

        var net_doc_id = instanceOf + "_" + doc_id[0].data;
        print("[WORKFLOW] down_right_and_store, find=", net_doc_id);

    }
    return {
        'right1': [
            {
                data: 'acl1',
                type: _String
                }]
    };
}

function is_in_docflow_and_set_if_true(process, task)
{
   var res = false
	
    if (task)
    {
		var doc_id = task.getVariableValue('docId');
		if (doc_id)
		{
			var instanceOf = getUri(process['v-wf:instanceOf']);

			var net_doc_id = instanceOf + "_" + doc_id[0].data;
			print("[WORKFLOW] is_in_docflow_and_set_if_true, find=", net_doc_id);

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
        'result': [
            {
                data: res,
                type: _Bool
                }]
    };	
		
   return res_out;	
}
