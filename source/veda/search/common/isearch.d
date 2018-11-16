/**
 * VQL Search interface
 */

module veda.search.common.isearch;

import veda.common.type, veda.core.common.type, veda.onto.individual;

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
    ResultCode result_code = ResultCode.NotReady;
}

interface Search
{
    public void reopen_db();
    public bool close_db();

    public int query(string user_uri, string filter, string sort, string db_names, int top, int limit,
                     ref Individual[] individuals, OptAuthorize op_auth, bool trace);

    public SearchResult query(string user_uri, string filter, string sort, string db_names, int from, int top, int limit, OptAuthorize op_auth, bool trace);
}
