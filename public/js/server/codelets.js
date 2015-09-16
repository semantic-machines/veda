"use strict";

function down_right_and_store(process, task)
{
    var doc_id = task.getVariableValue('docId');

    if (doc_id)
    {
        var instanceOf = getUri(process['v-wf:instanceOf']);

        var net_doc_id = instanceOf + "_" + doc_id[0].data;
        //print("[WORKFLOW]:down_right_and_store, find=", net_doc_id);

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
    //print("[WORKFLOW]:restore_right, task=", toJson(task));
    //print("[WORKFLOW]:restore_right function RESTORE RIGHT IS NOT IMPLIMENTED");
    var right = task.getVariableValue('originalRights');
    //print("[WORKFLOW]:restore_right ", toJson(right));
    return {
        'result': [
            {
                data: 'Ok',
                type: _String
                }]
    };
}

function is_exists_net_executor (process)
{
	var res = process.getExecutor () !== undefined;
	
	    var res_out = {
        'res':
        {
            data: res,
            type: _Bool
        }
    };

    return res_out;
}

function get_type_of_docId(process, task)
{
    var res = '?';

    if (task)
    {
        var doc_id = task.getVariableValue('docId');
        if (doc_id)
        {
            var doc = get_individual(process.ticket, doc_id[0].data);

            if (doc)
            {
                res = doc['rdf:type'][0].data;
            }
        }

    }

    var res_out = {
        'res':
        {
            data: res,
            type: _Uri
        }
    };
	
    return res_out;
}

function is_in_docflow_and_set_if_true(process, task)
{
    var res = false;

    if (task)
    {
        var doc_id = task.getVariableValue('docId');
        if (doc_id)
        {
            var instanceOf = getUri(process.src_data['v-wf:instanceOf']);

            var net_doc_id = instanceOf + "_" + doc_id[0].data;
            //print("[WORKFLOW]:is_in_docflow_and_set_if_true, find=", net_doc_id);

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

function distribution(process, task)
{
}

function create_use_transformation (process, task)
{
	var new_items_uri = [];

    if (task)
    {
		var src_doc_id = task.getVariableValue('src_uri');		
		var transform_link = task.getVariableValue('transformation_uri');		
		
		if (transform_link)
		{
			var transform = get_individual(ticket, transform_link);
			if (transform)
			{
				var document = get_individual(ticket, src_doc_id);
				if (document)
				{
					var new_items = transformation(ticket, document, transform, null, null);
					for (var i = 0; i < new_items.length; i++)
					{
						put_individual(ticket, new_items[i], _event_id);
						new_items_uri.push(
						{
							data: new_items[i]['@'],
							type: _Uri
						});
					}
				}
			}
		
		}
	}		
	
	var res_out = {
		'res': new_items_uri
    };
    return res_out;	
}	
