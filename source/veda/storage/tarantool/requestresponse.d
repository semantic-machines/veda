module veda.storage.tarantool.requestresponse;

private
{
    import veda.common.type;
}

class RequestResponse
{
    ResultCode   common_rc;
    ResultCode[] op_rc;
    string[]     binobjs;
    ubyte[]      rights;
}