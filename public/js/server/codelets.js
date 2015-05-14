"use strict";

function down_right_and_store(process, task)
{
    var doc_id = task.getVariableValue('docId');

    if (doc_id)
    {
        var instanceOf = getUri(process['v-wf:instanceOf']);

        var net_doc_id = instanceOf + "_" + doc_id[0].data;
        print("[WORKFLOW]:down_right_and_store, find=", net_doc_id);

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
    print("[WORKFLOW]:restore_right, task=", toJson(task));
    print("[WORKFLOW]:restore_right function RESTORE RIGHT IS NOT IMPLIMENTED");
    var right = task.getVariableValue('originalRights');
    print("[WORKFLOW]:restore_right ", toJson(right));
    return {
        'result': [
            {
                data: 'Ok',
                type: _String
                }]
    };
}

function is_in_docflow_and_set_if_true(process, task)
{
    var res = false;

    if (task)
    {
        var doc_id = task.getVariableValue('docId');
        if (doc_id)
        {
            var instanceOf = getUri(process['v-wf:instanceOf']);

            var net_doc_id = instanceOf + "_" + doc_id[0].data;
            print("[WORKFLOW]:is_in_docflow_and_set_if_true, find=", net_doc_id);

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
