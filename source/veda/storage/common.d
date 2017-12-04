module veda.storage.common;

import veda.common.type;

interface ReadStorage
{
    public string find(OptAuthorize op_auth, string user_uri, string uri, bool return_value = true);
    public void open();
    public void reopen();
    public void close();
    public long count_entries();
}

interface Storage : ReadStorage
{
    public ResultCode put(OptAuthorize op_auth, string user_id, string in_key, string in_value, long op_id);
    public ResultCode remove(OptAuthorize op_auth, string user_id, string in_key);
}

