module filters.filter_01;

import std.stdio, core.stdc.stdlib, std.uuid, std.algorithm, std.typecons, std.json, std.conv, std.string;
import veda.util.queue, veda.common.logger, veda.onto.individual, veda.onto.resource, veda.core.impl.app_context_creator_rlmdb;
import veda.storage.lmdb.lmdb_driver, veda.storage.lmdb.lmdb_header, veda.storage.common, veda.common.type, veda.onto.bj8individual.individual8json;

long t1_count, t2_count;
public void check_links_01(string data, ref Queue queue_new, LmdbDriver individual_lmdb_driver, Logger log)
{
    Individual imm;

    if (queue_new is null)
    {
        queue_new = new Queue("./tmp/uris", "uris", Mode.RW, log);
        queue_new.open();
        queue_new.get_info_push(0);
    }

    if (data !is null && imm.deserialize(data) < 0)
    {
        log.trace("ERR! read in queue: invalid individual:[%s]", data);
    }
    else
    {
        string type = imm.getFirstLiteral("type");

        if (type == "v-s:PermissionStatement")
        {
            t1_count++;

            string     new_bin = imm.getFirstLiteral("new_state");
            Individual new_indv;

            if (new_bin !is null && new_indv.deserialize(new_bin) < 0)
            {
                log.trace("ERR! read in queue, new binobj is individual:[%s]", new_bin);
            }
            else
            {
                string permission_subject_uri = new_indv.getFirstLiteral("v-s:permissionSubject");

                data = individual_lmdb_driver.get_binobj(permission_subject_uri);
                if (data is null)
                {
                    log.trace("permission_subject %s not found", permission_subject_uri);
                    t2_count++;
                    writefln("WARN! defect permission: uri=%-40s, all count=%d, defect count=%d", new_indv.uri,
                             t1_count,
                             t2_count);
                    queue_new.push(new_indv.uri);
                }
                else
                {
                    string permission_object_uri = new_indv.getFirstLiteral("v-s:permissionObject");

                    data = individual_lmdb_driver.get_binobj(permission_object_uri);
                    if (data is null)
                    {
                        log.trace("permission_object %s not found", permission_object_uri);
                        t2_count++;
                        writefln("WARN! defect permission: uri=%-40s, all count=%d, defect count=%d", new_indv.uri,
                                 t1_count,
                                 t2_count);
                        queue_new.push(new_indv.uri);
                    }
                }
            }
        }
    }
}

