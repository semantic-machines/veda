"use strict";

/*
 *   обработка формы решения пользователя
 */
function prepare_decision_form(ticket, document)
{
    try
    {
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

        var f_executor = _work_order['v-wf:executor'];
        var executor;

        if (f_executor && f_executor.length > 0)
            executor = f_executor[0];

        //        print ("@@@executor=", toJson (executor));	
        //print("[WORKFLOW][DF1].1");

        var f_forWorkItem = _work_order['v-wf:forWorkItem'];
        var work_item = get_individual(ticket, getUri(f_forWorkItem));
        if (!work_item) return;

        var forProcess_uri = getUri(work_item['v-wf:forProcess']);
        var _process = get_individual(ticket, forProcess_uri);
        if (!_process) return;

        var trace_journal_uri = create_new_trace_subjournal(forProcess_uri, work_item, "prepare_decision_form:" + decision_form['@'], 'v-wf:DecisionFormStarted')

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

        var process_output_vars = transformation(ticket, decision_form, transform, executor, f_onWorkOrder);

        //print("[WORKFLOW][DF1].5 transform_result=", toJson(process_output_vars));
        var new_vars = store_items_and_set_minimal_rights(ticket, process_output_vars);

        if (process_output_vars.length > 0)
        {
            _work_order['v-wf:outVars'] = new_vars;
            put_individual(ticket, _work_order, _event_id);

            document['v-wf:isCompleted'] = [newBool(true)];
            put_individual(ticket, document, _event_id);

            //print("[WORKFLOW][DF1].5 completedExecutorJournalMap");
            mapToJournal(net_element['v-wf:completedExecutorJournalMap'], ticket, _process, work_item, _work_order, null, getJournalUri(_work_order['@']));
            //print("[WORKFLOW][DF1].6 completedExecutorJournalMap");
        }
    }
    catch (e)
    {
        print(e.stack);
    }

}


/*
 *   обработка рабочего задания
 */
function prepare_work_order(ticket, document)
{
    try
    {
        var _work_order = document;

        var f_executor = document['v-wf:executor'];
        var executor = get_individual(ticket, getUri(f_executor));
        if (f_executor && !executor) return;

        var f_forWorkItem = getUri(document['v-wf:forWorkItem']);
        var work_item = get_individual(ticket, f_forWorkItem);
        if (!work_item) return;

        var forProcess_uri = getUri(work_item['v-wf:forProcess']);
        var _process = get_individual(ticket, forProcess_uri);
        if (!_process) return;

        var trace_journal_uri = create_new_trace_subjournal(f_forWorkItem, _work_order, "prepare_work_order:" + _work_order['@'], 'v-wf:WorkOrderStarted')
        if (trace_journal_uri)
            traceToJournal(ticket, trace_journal_uri, "обработка рабочего задания", toJson(document));

        var journal_uri;

        var f_inVars = work_item['v-wf:inVars'];
        if (!f_inVars)
            f_inVars = [];

        var f_process_inVars = _process['v-wf:inVars'];

        var forNetElement = work_item['v-wf:forNetElement'];
        var net_element = get_individual(ticket, getUri(forNetElement));
        if (!net_element) return;

        var f_local_outVars = document['v-wf:outVars'];
        var task_output_vars = [];

        var f_useSubNet = document['v-wf:useSubNet'];

        if (!f_local_outVars)
        {
            journal_uri = create_new_subjournal(f_forWorkItem, _work_order['@'], net_element['rdfs:label'], 'v-wf:WorkOrderStarted')
                // берем только необработанные рабочие задания
            if (!executor)
            {
                //print("[WORKFLOW][WO.2] executor not defined");

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
                    task_output_vars = create_and_mapping_variables(ticket, net_element['v-wf:completedMapping'], _process, work_item, null, null, true, trace_journal_uri, 'v-wf:completedMapping');
                    //print("[PWO].completedMapping1, task_output_vars=", toJson(task_output_vars));
                }

                if (task_output_vars.length == 0)
                {
                    task_output_vars.push(
                    {
                        data: 'v-wf:complete',
                        type: _Uri
                    });
                }
                else
                {
                    //mapToJournal(net_element['v-wf:completedJournalMap'], ticket, _process, work_item);				
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
                    var codelet_output_vars = eval(expression);
                    if (codelet_output_vars.length > 0)
                    {
                        var localVariablesUri = store_items_and_set_minimal_rights(ticket, codelet_output_vars);
                        _work_order['v-wf:outVars'] = localVariablesUri;
                        put_individual(ticket, _work_order, _event_id);
                    }
                    /*
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
                                   getStrings(netElement['rdfs:label'])     // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
                                        task_output_vars = create_and_mapping_variables(ticket, net_element['v-wf:completedMapping'], _process, work_item, null, result0, true, trace_journal_uri, 'v-wf:completedMapping');
                                        print("[PWO]completedMapping2, task_output_vars=", toJson(task_output_vars));
                                    }

                                    if (task_output_vars.length > 0)
                                    {
                                        document['v-wf:outVars'] = task_output_vars;
                                        put_individual(ticket, document, _event_id);
                                    }
                    */

                } // end [is codelet]        
                else if (is_exist(executor, 'rdf:type', 'v-s:Appointment') && !f_useSubNet)
                {
                    //print("[WORKFLOW][WO2] is USER, executor=" + getUri(f_executor));
                    //           //print("work_item.inVars=", toJson(f_inVars));
                    //           //print("process.inVars=", toJson(f_process_inVars));

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
                    //print ("@@@1 net_element['v-wf:startingExecutorJournalMap']=", toJson (net_element['v-wf:startingExecutorJournalMap']), ", @=", net_element['@']));
                    mapToJournal(net_element['v-wf:startingExecutorJournalMap'], ticket, _process, work_item, _work_order, null, journal_uri, trace_journal_uri, 'v-wf:startingExecutorJournalMap');

                    var transform_link = getUri(net_element['v-wf:startDecisionTransform']);
                    if (!transform_link) return;
                    var transform = get_individual(ticket, transform_link);
                    if (!transform) return;

                    var transform_result = transformation(ticket, work_item_inVars, transform, f_executor, newUri(document['@']));

                    if (trace_journal_uri)
                        traceToJournal(ticket, trace_journal_uri, "v-wf:startDecisionTransform", "transform_result=" + toJson(transform_result));

                    var decisionFormList = [];

                    if (transform_result)
                    {
                        for (var i = 0; i < transform_result.length; i++)
                        {
                            put_individual(ticket, transform_result[i], _event_id);
                            decisionFormList.push(
                            {
                                data: transform_result[i]['@'],
                                type: _Uri
                            });

                            // выдадим права отвечающему на эту форму
                            var employee = executor['v-s:employee'];
                            if (employee)
                            {
                                //print("[WORKFLOW][WO2.2] employee=" + toJson(employee));

                                addRight(ticket, [can_read, can_update], employee[0].data, transform_result[i]['@']);
                            }
                        }
                    }

                    var add_to_document = {
                        '@': document['@'],
                        'v-wf:decisionFormList': decisionFormList
                    };
                    add_to_individual(ticket, add_to_document, _event_id);
                    _work_order['v-wf:decisionFormList'] = decisionFormList;

                    mapToMessage(net_element['v-wf:startingMessageMap'], ticket, _process, work_item, _work_order, null, journal_uri, trace_journal_uri, 'v-wf:startingMessageMap');

                    //print("[WORKFLOW][WO2.3] transform_result=" + toJson(transform_result));
                }
                if (is_exist(executor, 'rdf:type', 'v-wf:Net') || f_useSubNet)
                {
                    create_new_subprocess(ticket, f_useSubNet, f_executor, net_element, f_inVars, document, trace_journal_uri);

                    //print("[WORKFLOW][WO21-1]");
                }
            }
        }

        var is_goto_to_next_task = false;

        // begin //////////////// скрипт сборки результатов (WorkOrder) ///////////////////////////////////////////
        var work_item_result = [];

        // найдем маппинг множественных результатов
        var wosResultsMapping = net_element['v-wf:wosResultsMapping'];

        var workOrderList = work_item['v-wf:workOrderList'];
        // проверяем есть ли результаты рабочих заданий
        if (workOrderList)
        {
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
                            if (wosResultsMapping)
                            {
                                // wosResultsMapping указан 
                            }
                            else
                            {
                                // складываем все результаты в локальную переменную					
                                var key, val;
                                var varName = _result["v-wf:variableName"];
                                if (varName)
                                    key = varName[0].data;

                                var varValue = _result["v-wf:variableValue"];
                                if (varValue)
                                    val = varValue;

                                if (val !== undefined && key !== undefined)
                                {
                                    el[key] = val;
                                    f_set = true;
                                }
                                //print("[WORKFLOW][WO3.3] result=" + toJson(result) + "");
                            }
                        }
                    }
                    if (f_set) work_item_result.push(el);

                }

            }
        }
        if (work_item_result.length == workOrderList.length)
            is_goto_to_next_task = true;
        else
        {
            if (trace_journal_uri)
                traceToJournal(ticket, trace_journal_uri, "[WO4.0] не все задания выполнены", "stop. work_item_result" + toJson(work_item_result) + ", workOrderList=", toJson(workOrderList));
        }

        // end //////////////// скрипт сборки результатов
        if (trace_journal_uri)
            traceToJournal(ticket, trace_journal_uri, "[WO4] work_item_result", toJson(work_item_result));

        var workItemList = [];

        if (is_goto_to_next_task)
        {
            journal_uri = getJournalUri(_work_order['@']);


            // переход к новой задаче  prepare[[wo][wo][wo]] ----> new [wi]

            if (work_item_result.length > 0)
            {
                if (work_item_result[0]['complete'])
                {
                    // если было пустое задание, то не журналируем
                }
                else
                {
                    //print("[WORKFLOW][WO4.0.0] completedJournalMap");
                    mapToJournal(net_element['v-wf:completedJournalMap'], ticket, _process, work_item, null, net_element['rdfs:label'], journal_uri);
                }
            }

            //print("[WORKFLOW][WO4.1] is_goto_to_next_task == true");
            if (net_element['v-wf:completedMapping'])
            {
                // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
                task_output_vars = create_and_mapping_variables(ticket, net_element['v-wf:completedMapping'], _process, work_item, null, work_item_result, true, trace_journal_uri, 'v-wf:completedMapping');
            }

            //            if (task_output_vars.length > 0)
            //            {
            //                document['v-wf:outVars'] = task_output_vars;
            //                put_individual(ticket, document, _event_id);
            //            }

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
                        //print("[WORKFLOW][WO8.1] work_item_result=" + toJson(work_item_result));
                        //print("[WORKFLOW][WO9] expression=" + toJson(expression));
                        if (expression)
                        {
                            try
                            {
                                var task_result = new WorkItemResult(work_item_result);
                                var task = new Context(work_item, ticket);
                                var process = new Context(_process, ticket);
                                var res1 = eval(expression);

                                if (trace_journal_uri)
                                    traceToJournal(ticket, trace_journal_uri, "in flow expression", toJson(expression) + ", res =" + toJson(res1));

                                if (res1 === true)
                                {
                                    // выполним переход по XOR условию								
                                    var nextNetElement = get_individual(ticket, getUri(flowsInto));

                                    if (nextNetElement)
                                    {
                                        //print("[WORKFLOW][WO10] create next work item for =" + nextNetElement['@']);
                                        var work_item_uri = create_work_item(ticket, forProcess_uri, nextNetElement['@'], work_item['@'], _event_id, trace_journal_uri);
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
                            catch (e)
                            {
                                if (trace_journal_uri)
                                    traceToJournal(ticket, trace_journal_uri, "in flow expression", toJson(expression) + ", ", toJson(e.stack));

                                print(e.stack);
                            }
                        }
                    }
                    else
                    {
                        if (!split)
                        {
                            // условия нет, выполним переход								
                            var nextNetElement = get_individual(ticket, getUri(flowsInto));

                            if (nextNetElement)
                            {
                                //print("[WORKFLOW][WO11] create next work item for =" + nextNetElement['@']);
                                var work_item_uri = create_work_item(ticket, forProcess_uri, nextNetElement['@'], work_item['@'], _event_id, trace_journal_uri);
                                workItemList.push(
                                {
                                    data: work_item_uri,
                                    type: _Uri
                                });
                            }
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

            if (task_output_vars.length > 0)
                work_item['v-wf:outVars'] = task_output_vars;

            put_individual(ticket, work_item, _event_id);
            //print("[WORKFLOW][WOe] update work_item=", toJson(work_item));

            remove_empty_branches_from_journal(journal_uri);
        }
    }
    catch (e)
    {
        print(e.stack);
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

    try
    {
        var forProcess = getUri(work_item['v-wf:forProcess']);
        var _process = get_individual(ticket, forProcess);
        if (!_process) return;

        var instanceOf = getUri(_process['v-wf:instanceOf']);
        var _net = get_individual(ticket, instanceOf);
        if (!_net) return;

        var forNetElement = document['v-wf:forNetElement'];
        var netElement = get_individual(ticket, getUri(forNetElement));
        if (!netElement) return;

        var trace_journal_uri;

        var isCompleted = document['v-wf:isCompleted'];
        if (isCompleted)
        {
            trace_journal_uri = get_trace_journal(document, _process)

            if (isCompleted[0].data === true)
            {
                if (trace_journal_uri)
                    traceToJournal(ticket, trace_journal_uri, "prepare_work_item:completed, exit", work_item['@']);

                remove_empty_branches_from_journal(getJournalUri(work_item['@']));

                return;
            }
        }

        trace_journal_uri = create_new_trace_subjournal(forProcess, work_item, netElement['@'] + "' - [" + getStrings(netElement['rdfs:label']) + "] - " + work_item['@'], 'v-wf:WorkItemStarted')

        var f_join = netElement['v-wf:join'];
        if (f_join && getUri(f_join) == "v-wf:AND")
        {
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
            //print("[WORKFLOW][PW00.3] and join is complete");
        }

        var is_completed = false;
        var workItemList = [];

        var is_goto_to_next_task = false;
        var task_output_vars = [];

        var journal_uri;

        if (is_exist(netElement, 'rdf:type', 'v-wf:Task'))
        {
            journal_uri = create_new_subjournal(forProcess, work_item['@'], netElement['rdfs:label'], 'v-wf:WorkItemStarted');

            if (trace_journal_uri)
                traceToJournal(ticket, trace_journal_uri, "Is task");

            //* выполнить стартовый маппинг переменных	
            var work_item__inVars = [];
            if (netElement['v-wf:startingMapping'])
            {
                work_item__inVars = create_and_mapping_variables(ticket, netElement['v-wf:startingMapping'], _process, document, null, null, true, trace_journal_uri, 'v-wf:startingMapping');
                if (work_item__inVars.length > 0)
                    document['v-wf:inVars'] = work_item__inVars;

                //var ctx = new Context(document, ticket);
                //ctx.print_variables('v-wf:inVars');
            }

            //* сформировать список исполнителей
            var executor_list = [];

            var f_subNet = netElement['v-wf:subNet'];
            var f_executor = netElement['v-wf:executor'];
            if (f_executor)
            {
                for (var i = 0; i < f_executor.length; i++)
                {
                    var executor = get_individual(ticket, f_executor[i].data);

                    if (!executor)
                    {
                        if (trace_journal_uri)
                            traceToJournal(ticket, trace_journal_uri, "исполнитель не найден", " uri=[" + f_executor[i].data + "]");
                    }
                    else
                    {
                        if (is_exist(executor, 'rdf:type', 'v-wf:ExecutorDefinition'))
                        {
                            // определение исполнителей посредством скрипта

                            var expression = getFirstValue(executor['v-wf:executorExpression']);
                            if (!expression) return;

                            var task = new Context(document, ticket);
                            var process = new Context(_process, ticket);

                            try
                            {
                                var result = eval(expression);

                                if (trace_journal_uri)
                                    traceToJournal(ticket, trace_journal_uri, "определение исполнителя [" + expression + "]", "executors=" + toJson(result));

                                if (result !== undefined && result.length > 0)
                                {
                                    for (var i3 = 0; i3 < result.length; i3++)
                                    {
                                        executor_list.push(result[i3]);
                                    }
                                }
                            }
                            catch (e)
                            {
                                if (trace_journal_uri)
                                    traceToJournal(ticket, trace_journal_uri, "исполнители не были определены [" + expression + "]", e.stack);
                            }

                        }
                        else
                        {
                            if (trace_journal_uri)
                                traceToJournal(ticket, trace_journal_uri, "установлен исполнитель ", "executor=" + toJson(f_executor[i].data));

                            executor_list.push(f_executor[i]);
                        }
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
            else
            {
                mapToJournal(netElement['v-wf:startingJournalMap'], ticket, _process, document, null, netElement['rdfs:label'], journal_uri);
            }

            var work_order_list = [];
            var work_order_uri_list = [];

            //* сформировать задания для исполнителей
            for (var i = 0; i < executor_list.length; i++)
            {
                var new_work_order_uri = genUri();

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

                if (trace_journal_uri)
                    new_work_order['v-wf:isTrace'] = newBool(true);

                if (executor_list[i] != null)
                    new_work_order['v-wf:executor'] = executor_list[i];

                //print("[PWI02-1] new order =" + toJson(new_work_order));

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
                addRight(ticket, [can_read], "v-wf:WorkflowReadUser", work_order_list[i]['@']);
            }

        } // end [Task]
        else if (is_exist(netElement, 'rdf:type', 'v-wf:InputCondition') || is_exist(netElement, 'rdf:type', 'v-wf:Condition'))
        {
            is_goto_to_next_task = true;
        } // end [InputCondition]
        else if (is_exist(netElement, 'rdf:type', 'v-wf:OutputCondition'))
        {
            if (trace_journal_uri)
                traceToJournal(ticket, trace_journal_uri, "Is output condition ", "");

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
                        task_output_vars.push(
                        {
                            data: 'v-wf:complete',
                            type: _Uri
                        });
                    }
                    else
                    {
                        // сохраняем результаты в v-wf:outVars в обрабатываемом рабочем задании
                        task_output_vars = create_and_mapping_variables(ticket, _net['v-wf:completedMapping'], _process, work_item, null, null, true, trace_journal_uri, 'v-wf:completedMapping');
                    }

                    if (task_output_vars.length > 0)
                    {
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
            
            var completeProcess = {
                '@': forProcess,
	            'v-wf:isCompleted': [{
                     data: true,
                     type: _Bool
                 }]
            };
            add_to_individual(ticket, completeProcess, _event_id);

        } // end [OutputCondition]    

        if (is_goto_to_next_task == true)
        {
            //print(":Is inputCondition or Condition");
            var hasFlows = netElement['v-wf:hasFlow'];
            if (hasFlows)
            {
                for (var i = 0; i < hasFlows.length; i++)
                {
                    var flow = get_individual(ticket, hasFlows[i].data);
                    if (!flow) continue;

                    ////print(":Flow: " + flow['@']);

                    var flowsInto = flow['v-wf:flowsInto'];
                    if (!flowsInto) continue;

                    var nextNetElement = get_individual(ticket, getUri(flowsInto));
                    if (!nextNetElement) continue;

                    var work_item_uri = create_work_item(ticket, forProcess, nextNetElement['@'], document['@'], _event_id, trace_journal_uri);
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

                    ////print("[WO12] document=", toJson(document));
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
    catch (e)
    {
        print(e.stack);
    }

}

/*
 *  обработка процесса
 */
function prepare_process(ticket, document)
{
    var deleted = document['v-s:deleted'];
    if (deleted)
        return;

    var _process = document;
    var trace_journal_uri = get_trace_journal(document, _process);

    if (trace_journal_uri)
        traceToJournal(ticket, trace_journal_uri, "prepare_process", document['@']);

    var inVars = _process['v-wf:inVars'];
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
                    addRight(ticket, [can_read], "v-wf:WorkflowReadUser", new_variable['@']);
                }
            }

        }
    }
    ////print("[PP04]");

    var workItemList = [];

    var f_consistsOf = net['v-wf:consistsOf'];
    if (f_consistsOf)
    {
        ////print("[PP05.0]");
        for (var i = 0; i < f_consistsOf.length; i++)
        {
            var net_consistsOf = get_individual(ticket, f_consistsOf[i].data);
            if (!net_consistsOf) continue;

            ////print("[PP05.1] net_consistsOf=", toJson(net_consistsOf));

            if (is_exist(net_consistsOf, 'rdf:type', 'v-wf:InputCondition'))
            {
                var work_item_uri = create_work_item(ticket, document['@'], net_consistsOf['@'], null, _event_id, trace_journal_uri);

                ////print("[PP05.2]");

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

    ////print("[PP0E]");
}



/*
 *  Обработка стартовой формы и создание экземпляра процесса.
 *  Условие запуска процесса: в стартовой форме не должно быть поля v-wf:isProcess.
 *  создается экземпляр v-wf:Process с заполненными переменными из текущей формы
 *  и экземпляр v-wf:WorkItem относящийся к v-wf:InputCondition
 */
function prepare_start_form(ticket, document)
{
    //print(":prepare_start_form #B, doc_id=" + document['@']);

    var isTrace = document['v-wf:isTrace'];
    if (isTrace && getFirstValue(isTrace) == true)
        isTrace = true;
    else
        isTrace = false;

    var hasStatusWorkflowif = document['v-s:hasStatusWorkflow'];
    if (hasStatusWorkflowif)
    {
        if (getUri(hasStatusWorkflowif) != 'v-s:ToBeSent')
        {
            //print("[WORKFLOW]:prepare_start_form, not ready to start.");
            return;
        }
    }
    else
        return;

    if (document['v-wf:isProcess'])
    {
        //print("[WORKFLOW]:prepare_start_form, already started.");
        return;
    }

    var new_process_uri = genUri();

    var author_uri;
    var ff = get_property_chain(ticket, document, 'v-s:author', 'v-s:employee');
    if (ff)
        author_uri = getUri(ff['field']);

    var forNet = document['v-wf:forNet'];
    var _net = get_individual(ticket, getUri(forNet));
    if (!_net) return;

    var new_vars = [];
    var transform_link = getUri(document['v-wf:useTransformation']);
    if (transform_link)
    {
        var transform = get_individual(ticket, transform_link);
        if (!transform) return;

        // формируем входящие переменные для нового процесса
        var process_inVars = transformation(ticket, document, transform, null, null);
        for (var i = 0; i < process_inVars.length; i++)
        {
            put_individual(ticket, process_inVars[i], _event_id);
            new_vars.push(
            {
                data: process_inVars[i]['@'],
                type: _Uri
            });

            addRight(ticket, [can_read], "v-wf:WorkflowReadUser", process_inVars[i]['@']);
        }
    }

    var new_process = {
        '@': new_process_uri,
        'rdf:type': newUri('v-wf:Process'),
        'v-wf:instanceOf': forNet
    };
    new_process['rdfs:label'] = [
    {
        data: "экземпляр маршрута :" + getFirstValue(_net['rdfs:label']),
        type: _String
    }];

    if (isTrace)
        new_process['v-wf:isTrace'] = newBool(true);

    if (new_vars.length > 0)
        new_process['v-wf:inVars'] = new_vars;

    var trace_journal_uri;

    if (isTrace)
    {
        trace_journal_uri = create_new_journal(ticket, getTraceJournalUri(new_process_uri), getJournalUri(document['v-wf:processedDocument'][0].data), _net['rdfs:label'], true);

        if (trace_journal_uri)
        {
            traceToJournal(ticket, trace_journal_uri, "started new process", toJson(new_process));
            new_process['v-wf:traceJournal'] = newUri(trace_journal_uri);
        }
    }

    put_individual(ticket, new_process, _event_id);

    create_new_journal(ticket, getJournalUri(new_process_uri), getJournalUri(document['v-wf:processedDocument'][0].data), _net['rdfs:label']);

    var jrId = genUri();
    var journalRecord = {
        '@': jrId,
        'rdf:type': newUri('v-s:ProcessStarted'),
        'v-s:actor': newUri(author_uri),
        'v-s:processJournal': newUri(getJournalUri(new_process_uri)),
        'v-wf:onProcess': newUri(new_process_uri),
        'v-s:onDocument': document['v-wf:processedDocument'],
        'v-s:created': [
        {
            data: new Date(),
            type: _Datetime
        }]
    };
    put_individual(ticket, journalRecord, _event_id);

    var membership = {
        '@': genUri(),
        'rdf:type': newUri('v-s:Membership'),
        'v-s:resource': newUri(new_process_uri),
        'v-s:memberOf': document['v-wf:processedDocument'],
        'rdfs:comment': newStr('Process is in document group')
    };
    put_individual(ticket, membership, _event_id);

    add_to_individual(ticket,
    {
        '@': document['v-wf:processedDocument'][0].data + 'j',
        'v-s:childRecord': newUri(jrId)
    }, _event_id);

    var add_to_document = {
        '@': document['@'],
        'v-wf:isProcess': newUri(new_process_uri)
    };

    add_to_individual(ticket, add_to_document, _event_id);

    // возьмем автора формы и выдадим ему полные права на процесс
    if (author_uri)
        addRight(ticket, [can_read, can_update, can_delete], author_uri, new_process_uri);

    addRight(ticket, [can_read], "v-wf:WorkflowReadUser", new_process_uri);
}
