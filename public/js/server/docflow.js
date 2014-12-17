"use strict";

/*
    обработка процесса
*/
function prepare_process (ticket, document)
{
//    print ("### prepare_process");
}

/*
    Обработка стартовой формы и создание экземпляра процесса.
    Условие запуска процесса: в стартовой форме должно быть заполнено поле v-wf:isProcess, 
    но экземпляра в хранилище еще не должно быть.
*/
function prepare_start_form (ticket, document)
{
    print ("### prepare_start_form #B _event_id=" + _event_id);

    var new_process_uri = getUri (document['v-wf:isProcess']);	    

    if (new_process_uri)
        return;
	
    var new_process_uri = guid ();

    var decomposition_link = getUri (document['v-wf:useDecomposition']);	    
    if (!decomposition_link)
        return;
		
    var decomposition = get_individual (ticket, decomposition_link);
    if (!decomposition)
	return;

    var forNet = document['v-wf:forNet'];
    var net = get_individual (ticket, getUri (forNet));
    if (!net)
	return;

    var new_vars = [];

    var new_process =
    {
	'@' : new_process_uri,
       	'rdf:type' : [{data: 'v-wf:Process', type : _Uri}],
	'v-wf:instanceOf' : forNet
    };

    // сформируем входящие переменные
    var process_input_vars = [];
    var mapping = decomposition['v-wf:startingMapping'];
    for (var i = 0; i < mapping.length; i++)
    {
	var map = get_individual (ticket, mapping[i].data);
	var expression = getFirstValue (map['v-wf:mappingExpression']);
	if (!expression)
	    continue;

	var res = eval (expression);

	var mapsTo = getUri (map['v-wf:mapsTo']);
	if (!mapsTo)
	    continue;

	var net_variable = get_individual (ticket, mapsTo);
	if (!net_variable)
	    continue;

	var variable_name = getFirstValue (net_variable['v-wf:variableName']) ; 

	var new_uri = guid ();
	var new_process_variable =
	{
       	    '@' : new_uri,
       	    'rdf:type' : [{data: 'v-wf:Variable', type : _Uri}],
	    'v-wf:variableName' : [{data: variable_name, type : _String}],
	    'v-wf:variableValue' : res
       	};

	new_vars[variable_name] = new_process_variable ;

       	put_individual (ticket, new_process_variable, _event_id);

	process_input_vars.push ({data: new_uri, type : _Uri});
    }
    if (process_input_vars.length > 0)
	new_process['v-wf:inputVariable'] = process_input_vars;

    // сформируем локальные переменные	
    var process_local_vars = [];
    var net_local_variable = net['v-wf:localVariable'];

    for (var i = 0; i < net_local_variable.length; i++)
    {
	var net_variable = get_individual (ticket, net_local_variable[i].data);
	if (!net_variable)
	    continue;

	var variable_name = getFirstValue (net_variable['v-wf:variableName']) ; 

	var new_uri = guid ();
	var new_process_variable =
	{
       	'@' : new_uri,
       	'rdf:type' : [{data: 'v-wf:Variable', type : _Uri}],
	'v-wf:variableName' : [{data: variable_name, type : _String}]
       	};
       	put_individual (ticket, new_process_variable, _event_id);

	process_local_vars.push ({data: new_uri, type : _Uri});
    }
    if (process_local_vars.length > 0)
	new_process['v-wf:localVariable'] = process_local_vars;

    put_individual (ticket, new_process, _event_id);    

    document['v-wf:isProcess'] = [{data: new_process_uri, type : _Uri}];
    put_individual (ticket, document, _event_id);  
  
    print ("### prepare_start_form #E");
}
