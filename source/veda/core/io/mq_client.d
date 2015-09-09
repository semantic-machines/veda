/**
 * MQ Client
 */

// TODO переделать на абстрактную модель, отправитель <---> получатель (socket --> uid)
module io.mq_client;

private import std.outbuffer;
private import veda.core.context;

interface mq_client
{
    // set callback function for listener ()
    void set_callback(void function(byte *data, int size, mq_client from_client, ref ubyte[], Context context = null) _message_acceptor);

    void get_count(out int cnt);

    void listener();

    int connect_as_listener(string[ string ] params);
    int connect_as_req(string[ string ] params);

    int send(char *messagebody, int message_size, bool send_more);
    string reciev();

    bool is_success();
    string get_fail_msg();
}

public bool check_params(string[] list_need_params, char[][ string ] params)
{
    foreach (param; list_need_params)
    {
        if ((param in params) is null)
        {
            return false;
        }
    }

    return true;
}
