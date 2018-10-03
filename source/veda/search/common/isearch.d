/**
 * VQL executor
 */

module veda.search.common.isearch;

private
{
    import veda.common.type, veda.onto.individual;
}

public struct SearchResult
{
    string[]   result;
    int        count;
    int        estimated;
    int        processed;
    long       cursor;
    long       total_time;
    long       query_time;
    long       authorize_time;
    ResultCode result_code = ResultCode.Not_Ready;
}

interface Search
{
    public void reopen_db();
    public bool close_db();

    public int query(string user_uri, string filter, string freturn, string sort, int top, int limit,
                     ref Individual[] individuals, OptAuthorize op_auth, bool trace);

    public SearchResult query(string user_uri, string filter, string freturn, string sort, int from, int top, int limit,
                              void delegate(string uri) prepare_element_event,
                              OptAuthorize op_auth, bool trace);

    public int query(string user_uri, string query_str, ref Individual[] res, OptAuthorize op_auth, bool trace);
}
