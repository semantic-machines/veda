"use strict";

/*
    обработка элемента сети
*/
function prepare_work_item(ticket, document)
{
    print("### prepare_work_item:" + document['@']);

    //    var fNet = document['v-wf:forProcess'];
    //    var net = get_individual (ticket, getUri (fNet));
    //    if (!net)
    //	return;

}

/*
    обработка процесса
*/
function prepare_process(ticket, document)
{
    print("### prepare_process:" + document['@']);

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
    print("### prepare_start_form #B _event_id=" + _event_id);

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

    var new_vars = [];

    var new_process = {
        '@': new_process_uri,
        'rdf:type': [
        {
            data: 'v-wf:Process',
            type: _Uri
        }],
        'v-wf:instanceOf': forNet
    };

    // сформируем входящие переменные
    var process_input_vars = [];
    var mapping = decomposition['v-wf:startingMapping'];
    for (var i = 0; i < mapping.length; i++)
    {
        var map = get_individual(ticket, mapping[i].data);
        var expression = getFirstValue(map['v-wf:mappingExpression']);
        if (!expression)
            continue;

        var res = eval(expression);

        var mapsTo = getUri(map['v-wf:mapsTo']);
        if (!mapsTo)
            continue;

        var net_variable = get_individual(ticket, mapsTo);
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
            }],
            'v-wf:variableValue': res
        };

        new_vars[variable_name] = new_process_variable;

        put_individual(ticket, new_process_variable, _event_id);

        process_input_vars.push(
        {
            data: new_uri,
            type: _Uri
        });
    }
    if (process_input_vars.length > 0)
        new_process['v-wf:inputVariable'] = process_input_vars;

    // сформируем локальные переменные	
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
        	var new_uri = guid();
        	var new_first_work_item = {
                '@': new_uri,
                'rdf:type': [
                {
                    data: 'v-wf:WorkItem',
                    type: _Uri
                }],
                'v-wf:forProcess': [
                {
                    data: new_process_uri,
                    type: _Uri
                }],
                'v-wf:forNetElement': [
                {
                    data: net_consistsOf['@'],
                    type: _Uri
                }]
        	};

                print("WORKFLOW:create work item:" + new_uri);

		put_individual(ticket, new_first_work_item, _event_id);
		break;
            }
        }
    }
    print("### prepare_start_form #E");
}

