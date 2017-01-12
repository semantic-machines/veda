"use strict";

function down_right_and_store(process, task)
{
    return change_rights(process, task, [{"data":"-r--"}]);
}

function change_rights(process, task, rightset)
{
    return change_rights_actor(process, task, [{"data":"-r--"}], 'actor');
}

function change_rights_actor(process, task, rightset, actor)
{
    try
    {
//print ("@JS down_right_and_store");
        var doc_id = process.getInputVariable('docId');
//print ("@JS doc_id=", toJson (doc_id));
//print ("@JS rightset=", toJson (rightset));
        var rset = []; 
        if (rightset[0].data.indexOf('r')>=0) {
            rset.push(can_read);
        }
        if (rightset[0].data.indexOf('u')>=0) {
            rset.push(can_update);
        }

        if (doc_id)
        {
            //print ("@JS1 executor=", toJson(process.getLocalVariable ('actor')));
            //print ("@JS2 executor=", toJson(process.getExecutor()));
    	    var executor = (process.getLocalVariable (actor))? process.getLocalVariable (actor) : process.getExecutor();
    
    	    executor = get_properties_chain (executor, [{$get:'v-s:occupation'}], executor);

	    if (!executor)
	    {
		print ("@JS executor undefined, actor=", process.getLocalVariable (actor));
	    }

    	    if (executor)
            	addRight(ticket, rset, getUri (executor), getUri (doc_id));	    

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

function complete_process(ticket, process, _event_id)
{
    change_process_status(ticket, process, 'v-wf:Completed', _event_id);
}

function interrupt_process(ticket, process, _event_id)
{
    change_process_status(ticket, process, 'v-wf:Interrupted', _event_id);    
}

function change_process_status(ticket, process, status, _event_id)
{
//    print('>>> '+toJson(process));
    var vars = process['v-wf:inVars'];
    if (!vars) return;
    for (var i = 0; i < vars.length; i++)
    {
        var variable = get_individual(process.ticket, vars[i].data);
        if (variable
            && variable['v-wf:variableName'][0]
            && variable['v-wf:variableName'][0].data == 'docId') {
            var doc = get_individual(ticket, variable['v-wf:variableValue'][0].data);
            if (doc['v-wf:isProcess'] && doc['v-wf:isProcess'][0].data == process['@']) {
                delete doc['v-wf:isProcess'];
                doc['v-wf:hasStatusWorkflow'] = newUri(status);
                put_individual(ticket, doc, _event_id);
            }
        }
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
    
// # 285
    return [get_new_variable('result', newUri(false))];
    
    try
    {
        var res = false;
        if (task)
        {
            var doc_id = task.getInputVariable('docId');
            if (doc_id)
            {
                var forProcess = getUri(task.src_data['v-wf:forProcess']);
                //print("[Z1Z] := "+toJson(forProcess));
                var process = get_individual(task.ticket, forProcess);
                //print("[Z2Z] := "+toJson(process));
                if (process)
                {
                    var instanceOf = getUri(process['v-wf:instanceOf']);

                    var net_doc_id = instanceOf + "_" + doc_id[0].data;
                    //print("[WORKFLOW]:is_in_docflow_and_set_if_true, find=", net_doc_id);

                    var in_doc_flow = get_individual(task.ticket, net_doc_id);
                    //print("[Z3Z] := "+toJson(in_doc_flow));

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
                        put_individual(task.ticket, new_doc, _event_id);
                        
                        var add_to_document = {
                            '@': doc_id[0].data,
                            'v-wf:isProcess': newUri(process['@'])
                        };
                        print('$ add_to_document >>'+toJson(add_to_document));
                        add_to_individual(ticket, add_to_document, _event_id);
                    }
                }
            }

        }

        return [get_new_variable('result', newUri(res))];
    }
    catch (e)
    {
        print(e.stack);
    }

}

function distribution(process, task)
{}

function add_value_to_document(process, task)
{
     try
     {
         var src;

         if (task)
         {
             var src_uri = task.getInputVariable('src_uri');
             var name_uri = task.getInputVariable('name_uri');
             var value = task.getInputVariable('value');

             var src;

             if (name_uri && value)
             {
                 src = get_individual(task.ticket, getUri(src_uri));
                 if (src)
                 {
                     name_uri = getUri(name_uri);
                     var ch_value = src[name_uri];

                     if (!ch_value)
                         ch_value = [];

                     for (var key in value)
                         ch_value.push(value[key]);

                     src[name_uri] = ch_value;
                     put_individual(ticket, src, _event_id);
                 }
             }
         }

         return [get_new_variable('res', src_uri)];
     }
     catch (e)
     {
         print(e.stack);
     }
}
 
function set_value_to_document(process, task)
{
     try
     {
         var src;

         if (task)
         {
             var src_uri = task.getInputVariable('src_uri');
             var name_uri = task.getInputVariable('name_uri');
             var value = task.getInputVariable('value');

             var src;

             if (name_uri && value)
             {
                 src = get_individual(task.ticket, getUri(src_uri));
                 if (src)
                 {
                     name_uri = getUri(name_uri);
                     src[name_uri] = value;
                     put_individual(ticket, src, _event_id);
                 }
             }
         }

         return [get_new_variable('res', src_uri)];
     }
     catch (e)
     {
         print(e.stack);
     }
} 
 
function create_use_transformation(process, task)
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
                        var new_items = transformation(task.ticket, document, transform, null, null, newUri (process.src_data['@']));
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
            //print('$ script_onto_rename:is_replace, ' + prev_doc['@'] + '->' + document['@']);
        }
        else
        {
            if (is_update)
            {
                put_individual(ticket, document, "");
            	//print('$ script_onto_rename:is_update, ' + prev_doc['@'] + '->' + document['@']);
            //            print('$ script_onto_rename:is_update, ' + toJson(prev_doc) + '->' + toJson(document));
            }
        }

        if (is_replace || is_update)
        {
//            print('$ script_onto_rename:is_update, ' + prev_doc['@'] + '->' + document['@']);
//                        print('$ script_onto_rename:is_update, ' + toJson(prev_doc) + '->' + toJson(document));
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
