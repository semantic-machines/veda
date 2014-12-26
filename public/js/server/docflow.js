"use strict";

/*
    обработка элемента сети
*/
function prepare_work_item(ticket, document)
{
    print("[WORKFLOW]: ### prepare_work_item:" + document['@']);

    var forProcess = getUri (document['v-wf:forProcess']);
    var process = get_individual (ticket, forProcess);
    if (!process)
    	return;

    print ("[WORKFLOW]:-------------------------------------------\r\n");
    print ("[WORKFLOW]:Process:" + process['@']);

    var forNetElement = document['v-wf:forNetElement'];
    var netElement = get_individual (ticket, getUri (forNetElement));
    if (!netElement)
    	return;

    print ("[WORKFLOW]:-------------------------------------------\r\n");
    print ("[WORKFLOW]:NetElement:" + netElement['@']);

    if (is_exist(netElement, 'rdf:type', 'v-wf:Task'))
    {
	print ("[WORKFLOW]:Is task");

	var executor_uri = getUri (netElement['v-wf:executor']);
	if (!executor_uri)
	    return;

	// выполнить маппинг переменных	
	print ("task: mapping vars start");
	var task_input_vars = create_and_mapping_input_variable (ticket, netElement, process);
	print ("task: mapping vars end");
        if (task_input_vars.length > 0)
    	    document['v-wf:inputVariable'] = task_input_vars;

	// взять исполнителя
	var executor_uri = getUri (netElement['v-wf:executor']);

	var executor = get_individual (ticket, executor_uri);
	if (!executor)
	    return;
		
	
	if (is_exist(executor, 'rdf:type', 'v-s:Codelet'))
	{
	    print ("executor=" + executor_uri + ", is codelet");

            var expression = getFirstValue(executor['v-s:script']);
    	    if (!expression)
        	return;

    	    var task = new Context (document, ticket);

    	    var res = eval(expression);
	
	    if (!res)
		return;	    
	}
    }
    else if (is_exist(netElement, 'rdf:type', 'v-wf:InputCondition'))
    {
	print ("[WORKFLOW]:Is input condition");
	var hasFlows = netElement['v-wf:hasFlow'];

	if (hasFlows)	
	{
	    for (var i = 0; i < hasFlows.length; i++)
	    {
		var flow = get_individual (ticket, hasFlows[i].data);
		if (!flow)
		    continue;

		print ("[WORKFLOW]:-------------------------------------------\r\n");
		print ("[WORKFLOW]:Flow: " + flow['@']);

		var flowsInto = flow['v-wf:flowsInto'];
		if (!flowsInto)
		    continue;

		var nextNetElement = get_individual (ticket, getUri (flowsInto));
		
		if (!nextNetElement)
		    continue;

		create_work_item (ticket, forProcess, nextNetElement['@'], _event_id);
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

    //    var forNet = document['v-wf:instanceOf'];
    //var net = get_individual (ticket, getUri (forNet));
    //    if (!net)
    //	return;
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
    if (!decomposition_link)
        return;

    var decomposition = get_individual(ticket, decomposition_link);
    if (!decomposition)
        return;

    var forNet = document['v-wf:forNet'];
    var net = get_individual(ticket, getUri(forNet));
    if (!net)
        return;

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
    var process_input_vars = create_and_mapping_input_variable (ticket, decomposition, new_process);
    if (process_input_vars.length > 0)
        new_process['v-wf:inputVariable'] = process_input_vars;

    // формируем локальные переменные	
    var process_local_vars = [];
    var net_local_variable = net['v-wf:localVariable'];
    if (net_local_variable)
    {
        for (var i = 0; i < net_local_variable.length; i++)
        {
            var net_variable = get_individual(ticket, net_local_variable[i].data);
            if (!net_variable)
                continue;

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
        if (process_local_vars.length > 0)
            new_process['v-wf:localVariable'] = process_local_vars;
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
            if (!net_consistsOf)
                continue;

            if (is_exist(net_consistsOf, 'rdf:type', 'v-wf:InputCondition'))
            {
		create_work_item (ticket, new_process_uri, net_consistsOf['@'], _event_id);
		break;
            }
        }
    }
    print("[WORKFLOW]:### prepare_start_form #E");
}

function create_work_item (ticket, process_uri, net_element_uri, _event_id)
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

function create_and_mapping_input_variable (ticket, mapping_src, work_item_with_variables)
{
    var process_input_vars = [];
    var mapping = mapping_src['v-wf:startingMapping'];
    if (!mapping)
	return [];

    for (var i = 0; i < mapping.length; i++)
    {
        var map = get_individual(ticket, mapping[i].data);
        var expression = getFirstValue(map['v-wf:mappingExpression']);
        if (!expression)
            continue;
	
	print ("[DOCFLOW]: expression=" + expression);
	var net = new Context (work_item_with_variables, ticket);
        var res = eval(expression);
	
	if (!res)
	    continue;

        var mapsTo = getUri(map['v-wf:mapsTo']);
        if (!mapsTo)
            continue;

        var net_variable = get_individual(ticket, mapsTo);
        if (!net_variable)
            continue;

        var variable_name = getFirstValue(net_variable['v-wf:variableName']);

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
            'v-wf:variableValue': res
        };

//        new_vars[variable_name] = process_variable;

        put_individual(ticket, new_variable, _event_id);

	print ("[WORKFLOW]:create variable: " + new_uri);

        process_input_vars.push(
        {
            data: new_uri,
            type: _Uri
        });
    }

    return process_input_vars;
}

function Context (_work_item, _ticket)
{
    this.work_item = _work_item;
    this.ticket = _ticket;

    this.getVariableValue = function (var_name)
    {
//	print ("work_item=" + toJson (this.work_item));
	var variables = this.work_item['v-wf:inputVariable'];
	print ("variables=" + toJson (variables));
	if (variables)
	{
    	    for (var i = 0; i < variables.length; i++)
    	    {
        	var variable = get_individual(this.ticket, variables[i].data);
        	if (!variable)
            	    continue;

        	var variable_name = getFirstValue(variable['v-wf:variableName']);
		if (variable_name == var_name)
		{
		    var val = variable['v-wf:variableValue'];

		    print ("[WORKFLOW]:getVariableValue: work_item=" + this.work_item['@'] + ", var_name=" + var_name + ", val=" + toJson (val));
		    return val;        		    
		}
	    }

	}
        print ("[WORKFLOW]:getVariableValue: work_item=" + this.work_item['@'] + ", var_name=" + var_name + ", val=undefined");
    }    
}

function is_in_docflow (doc_id, ticket)
{
    print ("is_in_docflow, docId=", toJson (doc_id));
}

