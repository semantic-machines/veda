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

// скрипт переименования онтологии
function onto_rename(ticket, document, execute_script)
{
    //    print ('$$$$$$$$$$$$$$ script_onto_rename:doc= ' + document['@']);
    try
    {
        //print ('$ script_onto_rename:execute_script= ' + toJson (execute_script));        
        if (document['@'] === execute_script['@'])
            return;

        var args_uris = execute_script['v-s:argument'];
        var args = loadVariablesUseField(ticket, args_uris);

        for (var idx in args_uris)
        {
            var arg_uri = args_uris[idx].data;
            if (arg_uri === document['@'])
                return;
        }

        var rename_template = args["rename_template"];
        var is_update = false;
        var is_replace = false;
        var prev_doc_uri = document['@'];
        var prev_doc = clone(document);
        var from_2_to = {};

        for (var idx in rename_template)
        {
            var template = rename_template[idx];

            var cc = template.split(',');
            if (!cc || cc.length != 2)
                continue;

            var from = cc[0];
            var to = cc[1];
            from_2_to[from] = to;

            var from_u = from.replace(':', '_');
            var to_u = to.replace(':', '_');

            if (from_u !== from)
                from_2_to[from_u] = to_u;
        }

        for (var key in document)
        {
            var values = document[key];
            if (key != '@')
            {
                for (var from in from_2_to)
                {
                    if (key === from)
                    {
                        var to = from_2_to[from];
                        document[to] = values;
                        delete document[from];
                    }
                }

                for (var idx in values)
                {
                    var value = values[idx];

                    for (var from in from_2_to)
                    {
                        if (value.type == _Uri || value.type == _String)
                        {
                            var to = from_2_to[from];
                            var new_str = replace_word(value.data, from, to);
                            if (new_str !== value.data)
                            {
                                is_update = true;
                                value.data = new_str;
                            }
                        }
                    }
                }
            }
            else
            {
                // replace in uri
                for (var from in from_2_to)
                {
                    var to = from_2_to[from];
                    //print ('values=', values, ', from=', from, ', to=', to); 
                    var new_str = replace_word(values, from, to);
                    if (new_str !== values)
                    {
                        is_replace = true;
                        document['@'] = new_str;
                    }
                }
            }
        }

        if (is_replace)
        {
            remove_individual(ticket, prev_doc_uri, "");
            put_individual(ticket, document, "");
        }
        else
        {
            if (is_update)
            {
                put_individual(ticket, document, "");
            }
        }

        if (is_replace || is_update)
        {
            //            print('$ script_onto_rename:is_update, ' + toJson(prev_doc) + '->' + toJson(document));
            print('$ script_onto_rename:is_update, ' + prev_doc['@'] + '->' + document['@']);
        }


    }
    catch (e)
    {
        if (typeof window === "undefined")
        {
            print(e.stack);
        }
        else
        {
            console.log(e.stack);
        }
    }
}
