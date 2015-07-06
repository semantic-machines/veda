"use strict";

/*
 *   обработка формы решения пользователя
 */
function prepare_decision_form(ticket, document)
{
    //print("[WORKFLOW][DF1] : ### ---------------------------- prepare_decision_form:" + document['@']);

    var decision_form = document;

    if (decision_form['v-wf:isCompleted'] && decision_form['v-wf:isCompleted'][0].data == true)
        return;

    //print("[WORKFLOW][DF1].0");
    var f_takenDecision = decision_form['v-wf:takenDecision'];
    if (!f_takenDecision)
        return;

    var f_onWorkOrder = document['v-wf:onWorkOrder'];
    var _work_order = get_individual(ticket, getUri(f_onWorkOrder));
    if (!_work_order) return;

    //print("[WORKFLOW][DF1].1");

    var f_forWorkItem = _work_order['v-wf:forWorkItem'];
    var work_item = get_individual(ticket, getUri(f_forWorkItem));
    if (!work_item) return;

    var forProcess_uri = getUri(work_item['v-wf:forProcess']);
    var _process = get_individual(ticket, forProcess_uri);
    if (!_process) return;

    //print("[WORKFLOW][DF1].2");

    var f_forNetElement = work_item['v-wf:forNetElement'];
    var net_element = get_individual(ticket, getUri(f_forNetElement));
    if (!net_element) return;

    //print("[WORKFLOW][DF1].3");

    var transform_link = getUri(net_element['v-wf:completeDecisionTransform']);
    if (!transform_link) return;
    var transform = get_individual(ticket, transform_link);
    if (!transform) return;

    //print("[WORKFLOW][DF1].4 document=", toJson(document));
    //print("[WORKFLOW][DF1].4 transform=", toJson(transform));
    //print("[WORKFLOW][DF1].4 _work_order=", toJson(_work_order));

    var process_output_vars = transformation(ticket, decision_form, transform, null, f_onWorkOrder);

    //print("[WORKFLOW][DF1].5 transform_result=", toJson(process_output_vars));
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
        _work_order['v-wf:outVars'] = new_vars;
        put_individual(ticket, _work_order, _event_id);

        document['v-wf:isCompleted'] = [
            {
                data: true,
                type: _Bool
                 }];
        put_individual(ticket, document, _event_id);

    //print("[WORKFLOW][DF1].5 completedExecutorJournalMap");
        mapToJournal(net_element['v-wf:completedExecutorJournalMap'], ticket, _process, work_item, _work_order);
    //print("[WORKFLOW][DF1].6 completedExecutorJournalMap");
    }
}


/*
 *   обработка рабочего задания
 */
function prepare_work_order(ticket, document)
{
    var _work_order = document;
    //print("[WORKFLOW][WO.1] : ### ---------------------------- prepare_work_order:" + document['@']);
    var f_executor = document['v-wf:executor'];
    var executor = get_individual(ticket, getUri(f_executor));
    //if (!executor) return;

    var f_forWorkItem = getUri(document['v-wf:forWorkItem']);
    var work_item = get_individual(ticket, f_forWorkItem);
    if (!work_item) return;

    var f_inVars = work_item['v-wf:inVars'];

    var forProcess_uri = getUri(work_item['v-wf:forProcess']);
    var _process = get_individual(ticket, forProcess_uri);
    if (!_process) return;

    var f_process_inVars = _process['v-wf:inVars'];

    var forNetElement = work_item['v-wf:forNetElement'];
    var net_element = get_individual(ticket, getUri(forNetElement));
    if (!net_element) return;

    var f_local_outVars = document['v-wf:outVars'];
    var task_output_vars = [];

    var f_useSubNet = document['v-wf:useSubNet'];

    // берем только необработанные рабочие задания
    if (!f_local_outVars)
    {
        // если исполнитель коделет
        if (!executor)
        {
            mapToJournal(net_element['v-wf:completedJournalMap'], ticket, _process, work_item);

            if (!net_element['v-wf:completedMapping'])
            {
                //print("[WORKFLOW][WO W6] v-wf:completedMapping not defined=", net_element['@']);
                task_output_vars.push(
                {
                    data: 'v-wf:complete',
                    type: _Uri
                });
            }
            else
            {
                // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
                task_output_vars = create_and_mapping_variables(ticket, net_element['v-wf:completedMapping'], _process, work_item, null, null, true);
                //print("[WORKFLOW][WO.2] task_output_vars=", toJson(task_output_vars));
            }

            if (task_output_vars.length > 0)
            {
                document['v-wf:outVars'] = task_output_vars;
                put_individual(ticket, document, _event_id);
            }
        }
        else
        {
            if (is_exist(executor, 'rdf:type', 'v-s:Codelet'))
            {
                //print("[WORKFLOW][WO1.2] executor=" + getUri(f_executor) + ", is codelet");

                var expression = getFirstValue(executor['v-s:script']);
                if (!expression) return;

                //print("[WORKFLOW][WO1.3] expression=" + expression);

                var task = new Context(work_item, ticket);
                var process = new Context(_process, ticket);
                var result0 = eval(expression);

                //print("[WORKFLOW][WO1.4] task: eval result=", toJson(result0));
                //print ("#2");
                //mapToJournal (net_element['v-wf:completedJournalMap'], ticket, _process, work_item);

                if (!net_element['v-wf:completedMapping'])
                {
                    //print("[WORKFLOW][WO W6] v-wf:completedMapping not defined=", net_element['@']);
                    task_output_vars.push(
                    {
                        data: 'v-wf:complete',
                        type: _Uri
                    });
                }
                else
                {
                    // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
                    task_output_vars = create_and_mapping_variables(ticket, net_element['v-wf:completedMapping'], _process, work_item, null, result0, true);
                    //print("[WORKFLOW][WO W6.1] task_output_vars=", toJson(task_output_vars));
                }

                if (task_output_vars.length > 0)
                {
                    document['v-wf:outVars'] = task_output_vars;
                    put_individual(ticket, document, _event_id);
                }


            } // end [is codelet]        
            else if (is_exist(executor, 'rdf:type', 'v-s:Appointment') && !f_useSubNet)
            {
                //print("[WORKFLOW][WO2] is USER, executor=" + getUri(f_executor));
                //            print("work_item.inVars=", toJson(f_inVars));
                //            print("process.inVars=", toJson(f_process_inVars));

                var work_item_inVars = [];
                for (var i = 0; i < f_inVars.length; i++)
                {
                    var indv = get_individual(ticket, f_inVars[i].data);
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

                //print("[WORKFLOW][WO2.0] transform_link=" + toJson(net_element['v-wf:startDecisionTransform']));
                //print("[WORKFLOW][WO2.1] work_item_inVars=" + toJson(work_item_inVars));                
                mapToJournal(net_element['v-wf:startingExecutorJournalMap'], ticket, _process, work_item, _work_order);

                var transform_link = getUri(net_element['v-wf:startDecisionTransform']);
                if (!transform_link) return;
                var transform = get_individual(ticket, transform_link);
                if (!transform) return;

                var transform_result = transformation(ticket, work_item_inVars, transform, f_executor, newUri(document['@']));

                for (var i = 0; i < transform_result.length; i++)
                {
                    put_individual(ticket, transform_result[i], _event_id);
                    // выдадим права отвечающему на эту форму
                    var employee = executor['v-s:employee'];
                    if (employee)
                    {
                        //print("[WORKFLOW][WO2.2] employee=" + toJson(employee));

                        addRight(ticket, [can_read, can_update], employee[0].data, transform_result[i]['@']);
                    }
                }

                //print("[WORKFLOW][WO2.3] transform_result=" + toJson(transform_result));
            }

            if (is_exist(executor, 'rdf:type', 'v-wf:Net') || f_useSubNet)
            {
                var use_net;

                if (f_useSubNet)
                    use_net = f_useSubNet;
                else
                    use_net = f_executor;

                //print("[WORKFLOW][WO2.4] executor= " + getUri(f_executor) + " used net= " + getUri(use_net));

                //var ctx = new Context(work_item, ticket);
                //ctx.print_variables ('v-wf:inVars');
                var _started_net = get_individual(ticket, getUri(use_net));
                if (_started_net)
                {
                    var new_process_uri = guid();

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
                                data: document['@'],
                                type: _Uri
      }]
                    };

                    var msg = "экземпляр маршрута :" + getFirstValue(_started_net['rdfs:label']) + ", запущен из " + getFirstValue(net_element['rdfs:label'])

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

                    //print("new_process=", toJson(new_process));
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
                //print("[WORKFLOW][WO21-1]");
            }
        }
    }

    var is_goto_to_next_task = false;

    // begin //////////////// скрипт сборки результатов (WorkOrder) ///////////////////////////////////////////
    var result = [];

    // найдем маппинг множественных результатов
    //var wosResultsMapping = net_element['v-wf:wosResultsMapping'];

    var workOrderList = work_item['v-wf:workOrderList'];
    // проверяем есть ли результаты рабочих заданий
    for (var i = 0; i < workOrderList.length; i++)
    {
        //print("[WORKFLOW][WO3.0] workOrder=" + toJson(workOrderList[i]) + "");
        var workOrder;
        if (workOrderList[i].data != document['@'])
            workOrder = get_individual(ticket, workOrderList[i].data);
        else
            workOrder = document;

        //print("[WORKFLOW][WO3.1] workOrder=" + toJson(workOrder) + "");

        var outVars = workOrder['v-wf:outVars'];
        if (outVars)
        {
            var el = {};
            el['workOrder'] = workOrder['@'];

            var f_set = false;
            for (var i1 = 0; i1 < outVars.length; i1++)
            {
                var _result = get_individual(ticket, outVars[i1].data);
                //print("[WORKFLOW][WO3.2] _result=" + toJson(_result) + "");
                if (_result)
                {
                    //if (wosResultsMapping)
                    //{
                    // wosResultsMapping указан 
                    //}
                    //else
                    {
                        // складываем все результаты в локальную переменную					
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
                            f_set = true;
                        }
                        //print("[WORKFLOW][WO3.3] result=" + toJson(result) + "");
                    }
                }
            }
            if (f_set) result.push(el);

        }

    }

    if (result.length == workOrderList.length)
        is_goto_to_next_task = true;
    //else
    //print("[WORKFLOW][WO4.0] не все задания выполнены, stop.");

    // end //////////////// скрипт сборки результатов
    //print("[WORKFLOW][WO4] result=" + toJson(result) + "");

    var workItemList = [];

    if (is_goto_to_next_task)
    {
        mapToJournal(net_element['v-wf:completedJournalMap'], ticket, _process, work_item);

        //print("[WORKFLOW][WO4.1] is_goto_to_next_task == true");
        if (net_element['v-wf:completedMapping'])
        {
            // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
            task_output_vars = create_and_mapping_variables(ticket, net_element['v-wf:completedMapping'], _process, work_item, null, result, true);
            //print("[WORKFLOW][WO6] task_output_vars=", toJson(task_output_vars));
        }

        if (task_output_vars.length > 0)
        {
            document['v-wf:outVars'] = task_output_vars;
            put_individual(ticket, document, _event_id);
        }

        // определим переход на следующие задачи в зависимости от результата
        // res должен быть использован при eval каждого из предикатов
        var hasFlows = net_element['v-wf:hasFlow'];
        if (hasFlows)
        {
            //print("[WORKFLOW][WO4.4]");
            var split = getUri(net_element['v-wf:split']);

            for (var i = 0; i < hasFlows.length; i++)
            {
                var flow = get_individual(ticket, hasFlows[i].data);
                if (!flow) continue;

                //print("[WORKFLOW][WO6]:Flow: " + flow['@']);

                var flowsInto = flow['v-wf:flowsInto'];
                if (!flowsInto) continue;

                var predicate = flow['v-wf:predicate'];
                if (predicate)
                {
                    //print("[WORKFLOW][WO8] predicate=" + toJson(predicate));
                    expression = getFirstValue(predicate);
                    //print("[WORKFLOW][WO9] expression=" + toJson(expression));
                    if (expression)
                    {
                        var res1 = eval(expression);
                        //print("[WORKFLOW][WO9.1] result=" + toJson(res1));
                        if (res1 === true)
                        {
                            // выполним переход по XOR условию								
                            var nextNetElement = get_individual(ticket, getUri(flowsInto));

                            if (nextNetElement)
                            {
                                //print("[WORKFLOW][WO10] create next work item for =" + nextNetElement['@']);
                                var work_item_uri = create_work_item(ticket, forProcess_uri, nextNetElement['@'], work_item['@'], _event_id);
                                workItemList.push(
                                {
                                    data: work_item_uri,
                                    type: _Uri
                                });
                            }

                            if (split == 'v-wf:XOR')
                                break;
                        }
                    }
                }
                else
                {
                    // условия нет, выполним переход								
                    var nextNetElement = get_individual(ticket, getUri(flowsInto));

                    if (nextNetElement)
                    {
                        //print("[WORKFLOW][WO11] create next work item for =" + nextNetElement['@']);
                        var work_item_uri = create_work_item(ticket, forProcess_uri, nextNetElement['@'], work_item['@'], _event_id);
                        workItemList.push(
                        {
                            data: work_item_uri,
                            type: _Uri
                        });
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

        if (workItemList.length > 0)
            work_item['v-wf:workItemList'] = workItemList;

        put_individual(ticket, work_item, _event_id);
        //print("[WORKFLOW][WOe] document=", toJson(document));
    }

}

/*
 *   обработка элемента сети
 * 			1. слияние
 * 			2. вычисление количества исполнителей, подготовка для них данных, запуск.
 * 			3. обработка результатов, ветвление 
 */
function prepare_work_item(ticket, document)
{
    var work_item = document;
    //print("[WORKFLOW]:prepare_work_item ### --------------------------------- " + document['@']);

    //print("[WORKFLOW][PWI]:----- NetElement:" + getUri(document['v-wf:forNetElement']) + ' -----');

    var isCompleted = document['v-wf:isCompleted'];

    if (isCompleted)
    {
        if (isCompleted[0].data === true)
        {
            //print("[WORKFLOW][PWI]:prepare_work_item, completed, exit");
            return;
        }
    }

    var forProcess = getUri(document['v-wf:forProcess']);
    var _process = get_individual(ticket, forProcess);
    if (!_process) return;

    var instanceOf = getUri(_process['v-wf:instanceOf']);
    var _net = get_individual(ticket, instanceOf);
    if (!_net) return;

    //print("[WORKFLOW]:Process=" + _process['@'] + ", net=" + _net['@']);

    var forNetElement = document['v-wf:forNetElement'];
    var netElement = get_individual(ticket, getUri(forNetElement));
    if (!netElement) return;

    var f_join = netElement['v-wf:join'];
    if (f_join && getUri(f_join) == "v-wf:AND")
    {
        //print("[WORKFLOW][PWI00.1] JOIN AND!");

        var in_flows = [];
        var task2flow = {};
        // найдем все flow входящие в эту задачу
        // 	обойдем все элементы сети, если это flow и идет к текущей задаче, то берем
        var f_consistsOf = _net['v-wf:consistsOf'];
        if (f_consistsOf)
        {
            for (var i = 0; i < f_consistsOf.length; i++)
            {
                var i_net_element = get_individual(ticket, f_consistsOf[i].data);
                if (!i_net_element) continue;

                if (is_exist(i_net_element, 'rdf:type', 'v-wf:Flow'))
                {
                    if (getUri(i_net_element["v-wf:flowsInto"]) == netElement['@'])
                    {
                        in_flows.push(i_net_element);
                        //print("[WORKFLOW][PW00.2] flow=", toJson (i_net_element));
                    }
                }
                else if (is_exist(i_net_element, 'rdf:type', 'v-wf:Task'))
                {
                    var f_hasFlow = i_net_element['v-wf:hasFlow'];
                    if (f_hasFlow)
                    {
                        for (var idx1 = 0; idx1 < f_hasFlow.length; idx1++)
                        {
                            task2flow[f_hasFlow[idx1].data] = i_net_element['@'];
                        }
                    }
                }
            }
        }

        // нужно обойти все дерево и найти незавершенные WorkItem соответсвующие текущей net_element
        var fne = find_in_work_item_tree(ticket, _process, 'v-wf:forNetElement', getUri(forNetElement));
        if (fne.length != in_flows.length)
            return;

        // проверим соответствие найденных WorkItem со схемой
        var and_join_count_complete = 0;

        for (var idx = 0; idx < in_flows.length; idx++)
        {
            var shema_out_task = task2flow[in_flows[idx]['@']];
            for (var idx1 = 0; idx1 < fne.length; idx1++)
            {
                var found_out_task = getUri(fne[idx1].parent['v-wf:forNetElement']);
                if (shema_out_task == found_out_task)
                {
                    and_join_count_complete++;
                    break;
                }
            }
        }

        if (and_join_count_complete != in_flows.length)
            return;

        //? остальные пути следует как то следует пометить?
        print("[WORKFLOW][PW00.3] and join is complete");
    }

    var is_completed = false;
    var workItemList = [];

    var is_goto_to_next_task = false;
    var task_output_vars = [];

    if (is_exist(netElement, 'rdf:type', 'v-wf:Task'))
    {
        //print("[WORKFLOW]:Is task");

        mapToJournal(netElement['v-wf:startingJournalMap'], ticket, _process, document);

        //* выполнить стартовый маппинг переменных	
        // print("[PWI01.2] task: start mapping vars");
        var work_item__inVars = [];
        if (netElement['v-wf:startingMapping'])
        {
            work_item__inVars = create_and_mapping_variables(ticket, netElement['v-wf:startingMapping'], _process, document, null, null, true);
            if (work_item__inVars.length > 0)
                document['v-wf:inVars'] = work_item__inVars;

            //var ctx = new Context(document, ticket);
            //ctx.print_variables('v-wf:inVars');
        }

        //print("work_item__inVars=", toJson(work_item__inVars));

        //* сформировать список исполнителей
        var executor_list = [];

        var f_subNet = netElement['v-wf:subNet'];
        var f_executor = netElement['v-wf:executor'];
        if (f_executor)
        {
            for (var i = 0; i < f_executor.length; i++)
            {
                var executor = get_individual(ticket, f_executor[i].data);

                if (is_exist(executor, 'rdf:type', 'v-wf:ExecutorDefinition'))
                {
                    // print("[PWI01-1] executor=" + f_executor[i].data + ", script defined");
                    // определение исполнителей посредством скрипта

                    var expression = getFirstValue(executor['v-s:script']);
                    if (!expression) return;

                    //// print(" expression=" + expression);

                    var task = new Context(document, ticket);
                    //            var net = new Context(_net, ticket);
                    var process = new Context(_process, ticket);
                    //var context = task;

                    var result = eval(expression);

                    //// print(" task: result of v-wf:ExecutorDefinition=", toJson(result));

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
                    // print("[PWI01-2] executor=" + f_executor[i].data);

                    executor_list.push(f_executor[i]);
                }
            }
        }
        else
        {
            if (f_subNet)
                executor_list.push(f_subNet[0]);
        }

        //* если не найдено ни одного исполнителя, то добавим null, 
        //* 	как индикатор для создания проходного(пустого) задания
        if (executor_list.length == 0)
            executor_list.push(null);

        // print("[PWI02] executor list =" + toJson(executor_list));

        var work_order_list = [];
        var work_order_uri_list = [];

        //* сформировать задания для исполнителей
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
     }]
            };

            if (f_subNet)
                new_work_order['v-wf:useSubNet'] = f_subNet;

            if (executor_list[i] != null)
                new_work_order['v-wf:executor'] = executor_list[i];

            work_order_list.push(new_work_order);
            work_order_uri_list.push(
            {
                data: new_work_order_uri,
                type: _Uri
            });
        }

        if (work_order_uri_list.length > 0)
            document['v-wf:workOrderList'] = work_order_uri_list;

        if (work_item__inVars > 0 || work_order_uri_list.length > 0)
            put_individual(ticket, document, _event_id);

        for (var i = 0; i < work_order_list.length; i++)
        {
            put_individual(ticket, work_order_list[i], _event_id);
        }

    } // end [Task]
    else if (is_exist(netElement, 'rdf:type', 'v-wf:InputCondition') || is_exist(netElement, 'rdf:type', 'v-wf:Condition'))
    {
        is_goto_to_next_task = true;
    } // end [InputCondition]
    else if (is_exist(netElement, 'rdf:type', 'v-wf:OutputCondition'))
    {
        // print("[PWI]:Is output condition");
        //var process = new Context(_process, ticket);
        //process.print_variables('v-wf:inVars');
        //process.print_variables('v-wf:outVars');

        var f_parent_work_order = _process['v-wf:parentWorkOrder'];
        if (f_parent_work_order)
        {
            var parent_work_order = get_individual(ticket, getUri(f_parent_work_order));
            if (parent_work_order)
            {
                if (!_net['v-wf:completedMapping'])
                {
                    // print("[PWI] #6");
                    task_output_vars.push(
                    {
                        data: 'v-wf:complete',
                        type: _Uri
                    });
                }
                else
                {
                    // print("[PWI] #7");
                    // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
                    task_output_vars = create_and_mapping_variables(ticket, _net['v-wf:completedMapping'], _process, work_item, null, null, true);
                }

                if (task_output_vars.length > 0)
                {
                    // print("[PWI] #8");
                    parent_work_order['v-wf:outVars'] = task_output_vars;
                    put_individual(ticket, parent_work_order, _event_id);
                }

            }
        }


        document['v-wf:isCompleted'] = [
            {
                data: true,
                type: _Bool
       }];

        is_completed = true;

    } // end [OutputCondition]    

    if (is_goto_to_next_task == true)
    {
        // print(":Is inputCondition or Condition");
        var hasFlows = netElement['v-wf:hasFlow'];
        if (hasFlows)
        {
            for (var i = 0; i < hasFlows.length; i++)
            {
                var flow = get_individual(ticket, hasFlows[i].data);
                if (!flow) continue;

                // print(":Flow: " + flow['@']);

                var flowsInto = flow['v-wf:flowsInto'];
                if (!flowsInto) continue;

                var nextNetElement = get_individual(ticket, getUri(flowsInto));
                if (!nextNetElement) continue;

                var work_item_uri = create_work_item(ticket, forProcess, nextNetElement['@'], document['@'], _event_id);
                workItemList.push(
                {
                    data: work_item_uri,
                    type: _Uri
                });
                document['v-wf:isCompleted'] = [
                    {
                        data: true,
                        type: _Bool
                 }];
                is_completed = true;

                // print("[WO12] document=", toJson(document));
            }
        }
    }

    if (workItemList.length > 0)
        document['v-wf:workItemList'] = workItemList;

    if (is_completed == true || workItemList.length > 0)
    {
        put_individual(ticket, document, _event_id);
    }

}

/*
 *  обработка процесса
 */
function prepare_process(ticket, document)
{
    var _process = document;

    // print("[PP01] prepare_process:" + document['@']);
    var inVars = _process['v-wf:inVars'];
    if (!inVars)
        inVars = [];

    // print("[PP02]");
    var instanceOf = document['v-wf:instanceOf'];
    var net = get_individual(ticket, getUri(instanceOf));
    if (!net) return;

    // print("[PP03]");
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
    }
    // print("[PP04]");

    var workItemList = [];

    var f_consistsOf = net['v-wf:consistsOf'];
    if (f_consistsOf)
    {
        // print("[PP05.0]");
        for (var i = 0; i < f_consistsOf.length; i++)
        {
            var net_consistsOf = get_individual(ticket, f_consistsOf[i].data);
            if (!net_consistsOf) continue;

            // print("[PP05.1] net_consistsOf=", toJson(net_consistsOf));

            if (is_exist(net_consistsOf, 'rdf:type', 'v-wf:InputCondition'))
            {
                var work_item_uri = create_work_item(ticket, document['@'], net_consistsOf['@'], null, _event_id);

                // print("[PP05.2]");

                workItemList.push(
                {
                    data: work_item_uri,
                    type: _Uri
                });

                break;
            }
        }

    }

    if (inVars.length > 0)
        document['v-wf:inVars'] = inVars;

    //var process = new Context(_process, ticket);
    //process.print_variables('v-wf:inVars');

    if (workItemList.length > 0)
        document['v-wf:workItemList'] = workItemList;

    if (inVars.length > 0 || workItemList.length > 0)
        put_individual(ticket, document, _event_id);

    // print("[PP0E]");
}

/*
 *  Обработка стартовой формы и создание экземпляра процесса.
 *  Условие запуска процесса: в стартовой форме не должно быть поля v-wf:isProcess.
 *  создается экземпляр v-wf:Process с заполненными переменными из текущей формы
 *  и экземпляр v-wf:WorkItem относящийся к v-wf:InputCondition
 */
function prepare_start_form(ticket, document)
{
    // print(":prepare_start_form #B, doc_id=" + document['@']);

    if (document['v-wf:isProcess'])
    {
        //print("[WORKFLOW]:prepare_start_form, already started.");
        return;
    }

    var new_process_uri = guid();

    var forNet = document['v-wf:forNet'];
    var _net = get_individual(ticket, getUri(forNet));
    if (!_net) return;

    var transform_link = getUri(document['v-wf:useTransformation']);
    if (!transform_link) return;

    var transform = get_individual(ticket, transform_link);
    if (!transform) return;

    // формируем входящие переменные для нового процесса
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

    var new_process = {
        '@': new_process_uri,
        'rdf:type': [
            {
                data: 'v-wf:Process',
                type: _Uri
          }],
        'v-wf:instanceOf': forNet
    };
    new_process['rdfs:label'] = [
        {
            data: "экземпляр маршрута :" + getFirstValue(_net['rdfs:label']),
            type: _String
      }];
    if (process_inVars.length > 0) new_process['v-wf:inVars'] = new_vars;

    put_individual(ticket, new_process, _event_id);
    //print("new_process=", toJson(new_process));

    create_new_journal(ticket, new_process_uri, _net['rdfs:label']);

    document['v-wf:isProcess'] = [
        {
            data: new_process_uri,
            type: _Uri
      }];
    put_individual(ticket, document, _event_id);
    //print("[WORKFLOW]:new_process:" + new_process['@']);
}
