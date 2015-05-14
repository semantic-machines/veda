"use strict";

/*
 *   обработка формы решения пользователя
 */
function prepare_decision_form(ticket, document)
{
    if (document['v-wf:isCompleted'] && document['v-wf:isCompleted'][0].data == true)
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

    print("[WORKFLOW][DF1].4 document=", toJson(document));
    print("[WORKFLOW][DF1].4 transform=", toJson(transform));
    print("[WORKFLOW][DF1].4 work_order=", toJson(work_order));

    var process_output_vars = transformation(ticket, document, transform, null, onWorkOrder);

    print("[WORKFLOW][DF1].5 transform_result=", toJson(process_output_vars));
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
        work_order['v-wf:outVars'] = new_vars;
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

    var work_item_inVars_uri = work_item['v-wf:inVars'];

    var forProcess_uri = getUri(work_item['v-wf:forProcess']);
    var process = get_individual(ticket, forProcess_uri);
    if (!process) return;

    var process_inVars_uri = process['v-wf:inVars'];

    var forNetElement = work_item['v-wf:forNetElement'];
    var netElement = get_individual(ticket, getUri(forNetElement));
    if (!netElement) return;

    var local_outVars = document['v-wf:outVars'];
	// берем только необработанные рабочие задания
    if (!local_outVars)
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

			var task_output_vars = [];
			
            if (!netElement['v-wf:completedMapping'])
            {
                print("[WORKFLOW][WO W6] v-wf:completedMapping not defined=", netElement['@']);
				task_output_vars.push (
					{
					data: 'v-wf:complete',
					type: _Uri
					}
				);
            }
			else
			{
				// сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
				task_output_vars = create_and_mapping_variables(ticket, netElement['v-wf:completedMapping'], process, work_item, result0);
				//print("[WORKFLOW][WO W6.1] task_output_vars=", toJson(task_output_vars));						
			}
			
			if (task_output_vars.length > 0)
			{	
				document['v-wf:outVars'] = task_output_vars;
				put_individual(ticket, document, _event_id);
			}
			

        } // end [is codelet]        
        else if (is_exist(executor, 'rdf:type', 'v-s:Appointment'))
        {
            print("[WORKFLOW][WO20] is USER, executor=" + getUri(executor_uri) + "");
            //print("work_item.inVars=", toJson(work_item_inVars_uri));
            //print("process.inVars=", toJson(process_inVars_uri));

            var work_item_inVars = [];
            for (var i = 0; i < work_item_inVars_uri.length; i++)
            {
                var indv = get_individual(ticket, work_item_inVars_uri[i].data);
                work_item_inVars.push(indv);
            }

            var prev_task;
            var i_work_item = work_item;

            while (!prev_task)
            {
                var previousWorkItem_uri = getUri(i_work_item['v-wf:previousWorkItem']);
                if (!previousWorkItem_uri)
                    break;

                var previous_work_item = get_individual(ticket, previousWorkItem_uri);
                if (!previous_work_item)
                    break;

                var prev_forNetElement_uri = getUri(previous_work_item['v-wf:forNetElement']);
                if (!prev_forNetElement_uri)
                    break;

                var prev_forNetElement = get_individual(ticket, prev_forNetElement_uri);
                if (!prev_forNetElement)
                    break;

                if (prev_forNetElement['rdf:type'][0].data == 'v-wf:Task')
                {
                    prev_task = previous_work_item['v-wf:forNetElement'];
                    break;
                }
                i_work_item = previous_work_item;
            }

// ? или сделать curTask и prevTask только для трансформации ?
                // ++ work_item_inVars: cur task id
                var var_ctid = {
                    '@': '-',
                    'rdf:type': [
                        {
                            data: 'v-wf:Variable',
                            type: _Uri
                    }],
                    'v-wf:variableName': [
                        {
                            data: "curTask",
                            type: _String
                    }],
                    'v-wf:variableValue': forNetElement
                };
                work_item_inVars.push(var_ctid);

            if (prev_task)
            {
                // ++ work_item_inVars: prev task id
                var_ctid = {
                    '@': '-',
                    'rdf:type': [
                        {
                            data: 'v-wf:Variable',
                            type: _Uri
                    }],
                    'v-wf:variableName': [
                        {
                            data: "prevTask",
                            type: _String
                    }],
                    'v-wf:variableValue': prev_task
                };
                work_item_inVars.push(var_ctid);
            }

            //print("[WORKFLOW][WO20.0] transform_link=" + toJson(netElement['v-wf:startResultTransform']));
            print("[WORKFLOW][WO20.1] work_item_inVars=" + toJson(work_item_inVars));


            var transform_link = getUri(netElement['v-wf:startResultTransform']);
            if (!transform_link) return;
            var transform = get_individual(ticket, transform_link);
            if (!transform) return;

            var transform_result = transformation(ticket, work_item_inVars, transform, executor_uri, newUri(document['@']));

            for (var i = 0; i < transform_result.length; i++)
            {
                put_individual(ticket, transform_result[i], _event_id);
                // выдадим права отвечающему на эту форму
                var employee = executor['v-s:employee'];
                if (employee)
                {
                    print("[WORKFLOW][WO20.3] employee=" + toJson(employee));

                    addRight(ticket, [can_read, can_update], employee[0].data, transform_result[i]['@']);
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
        print("[WORKFLOW][WO30.0] workOrder=" + toJson (workOrderList[i]) + "");        
        var workOrder;
        if (workOrderList[i].data != document['@'])
            workOrder = get_individual(ticket, workOrderList[i].data);
        else
            workOrder = document;

        print("[WORKFLOW][WO30.1] workOrder=" + toJson (workOrder) + "");        

        var outVars = workOrder['v-wf:outVars'];
        if (outVars)
        {
            var _result = get_individual(ticket, outVars[0].data);
            print("[WORKFLOW][WO30.2] _result=" + toJson (_result) + "");        
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
                    print("[WORKFLOW][WO30.3] result=" + toJson (result) + "");        
                }
            }
        }

    }
    print("[WORKFLOW][WO1-20]");

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
        var work__item_inVars = create_and_mapping_variables(ticket, netElement['v-wf:startingMapping'], process, document, null);
        if (work__item_inVars.length > 0) document['v-wf:inVars'] = work__item_inVars;
        print("work__item_inVars=", toJson(work__item_inVars));
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
    var inVars = document['v-wf:inVars'];
    if (!inVars)
        inVars = [];

    var instanceOf = document['v-wf:instanceOf'];
    var net = get_individual(ticket, getUri(instanceOf));
    if (!net) return;

    // создадим переменные с областью видимости данного процесса (v-wf:varDefineScope = v-wf:Net)
    var variables = net['v-wf:localVariable'];
    if (variables)
    {
        for (var i = 0; i < variables.length; i++)
        {
            var def_variable = get_individual(ticket, variables[i].data);
            if (!def_variable) continue;

            var variable_scope = getUri(def_variable['v-wf:varDefineScope']);
            if (!variable_scope) continue;

            if (variable_scope == 'v-wf:Net')
            {
                var new_variable = generate_variable(ticket, def_variable, null, document, null, null);
                if (new_variable)
                {
                    put_individual(ticket, new_variable, _event_id);
                    inVars.push(
                    {
                        data: new_variable['@'],
                        type: _Uri
                    });
                }
            }

        }

        if (inVars.length > 0)
        {
            document['v-wf:inVars'] = inVars;
            put_individual(ticket, document, _event_id);
        }
    }
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
    var process_inVars = transformation(ticket, document, transform, null, null);
    var new_vars = [];
    for (var i = 0; i < process_inVars.length; i++)
    {
        put_individual(ticket, process_inVars[i], _event_id);
        new_vars.push(
        {
            data: process_inVars[i]['@'],
            type: _Uri
        });
    }
    if (process_inVars.length > 0) new_process['v-wf:inVars'] = new_vars;

    print("new_process=", toJson(new_process));

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
