"use strict";

function down_right_and_store(task)
{
    try
    {
        var doc_id = task.getInputVariable('docId');

        if (doc_id)
        {
            var instanceOf = getUri(process['v-wf:instanceOf']);

            var net_doc_id = instanceOf + "_" + doc_id[0].data;
            //print("[WORKFLOW]:down_right_and_store, find=", net_doc_id);

        }
        return [get_new_variable('right', newStr('acl1'))];
    }
    catch (e)
    {
        print(e.stack);
    }
}

function restore_right(task)
{
    try
    {
        //print("[WORKFLOW]:restore_right, task=", toJson(task));
        //print("[WORKFLOW]:restore_right function RESTORE RIGHT IS NOT IMPLIMENTED");
        var right = task.getInputVariable('originalRights');
        //print("[WORKFLOW]:restore_right ", toJson(right));
        return [get_new_variable('result', newStr('Ok'))];

    }
    catch (e)
    {
        print(e.stack);
    }
}

function is_exists_net_executor(process)
{
    try
    {
        var res = process.getExecutor() !== undefined;
        return [get_new_variable('res', newBool(res))];
    }
    catch (e)
    {
        print(e.stack);
    }
}

function get_type_of_docId(task)
{
    try
    {
        var res = '?';

        if (task)
        {
            var doc_id = task.getInputVariable('docId');
            if (doc_id)
            {
                var doc = get_individual(task.ticket, doc_id[0].data);

                if (doc)
                {
                    res = doc['rdf:type'][0].data;
                }
            }

        }

        return [get_new_variable('res', newUri(res))];
    }
    catch (e)
    {
        print(e.stack);
    }

}

function is_in_docflow_and_set_if_true(task)
{
    try
    {
        var res = false;

        if (task)
        {
            var doc_id = task.getInputVariable('docId');
            if (doc_id)
            {
                var forProcess = getUri(task.src_data['v-wf:forProcess']);
                var process = get_individual(task.ticket, forProcess);
                if (process)
                {
                    var instanceOf = getUri(_process['v-wf:instanceOf']);

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

        }

        return [get_new_variable('res', newUri(res))];
    }
    catch (e)
    {
        print(e.stack);
    }

}

function distribution(process, task)
{}

function create_use_transformation(task)
{
    try
    {
        var new_items_uri = [];

        if (task)
        {
            var src_doc_id = task.getInputVariable('src_uri');
            var transform_link = task.getInputVariable('transformation_uri');

            if (transform_link)
            {
                var transform = get_individual(task.ticket, getUri(transform_link));
                if (transform)
                {
                    var document = get_individual(task.ticket, getUri(src_doc_id));
                    if (document)
                    {
                        var new_items = transformation(task.ticket, document, transform, null, null);
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

        return [get_new_variable('res', new_items_uri)];
    }
    catch (e)
    {
        print(e.stack);
    }

}
