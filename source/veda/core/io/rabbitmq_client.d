/**
 * RabbitMQ Client
 */

module io.rabbitmq_client;

private import core.thread, std.stdio, std.c.string, std.c.stdlib, std.datetime, std.outbuffer, std.uuid, std.conv;

private import bind.librabbitmq_headers;
private import io.mq_client;
private import util.logger;
private import veda.core.context;

alias void listener_result;

logger     log;

static this()
{
    log = new logger("rabbitmq", "log", null);
}

class rabbitmq_client : mq_client
{
    private amqp_connection_state_t conn;
    private int                     sockfd;

    private string                  fail;
    private bool                    is_success_status = false;
    private amqp_bytes_t            queuename;

    bool is_success()
    {
        return is_success_status;
    }

    string get_fail_msg()
    {
        return fail;
    }

    void function(byte *txt, int size, mq_client from_client, ref ubyte[] out_data, Context context = null) message_acceptor;

    int  count  = 0;
    bool isSend = false;

    this()
    {
    }

    ~this()
    {
        die_on_amqp_error(amqp_channel_close(&conn, 1, AMQP_REPLY_SUCCESS), "Closing channel");
        die_on_amqp_error(amqp_connection_close(&conn, AMQP_REPLY_SUCCESS), "Closing connection");
//		die_on_error(amqp_destroy_connection(&conn), "Ending connection");
    }

    int connect_as_listener(string[ string ] params)
    {
        if (log is null)
            log = new logger("rabbitmq", "log", null);

        //		string[] need_params = ["port", "hostname", "queuename", "vhost", "login", "credentional"];
        //		if(check_params(need_params, params) == false)
        //		{
        //			fail = "не достаточно параметров, необходимо:" ~ text(need_params) ~ ", представлено:" ~ text(params);
        //			return -1;
        //		} else
        //		{
        writeln("###1 params=", params);
        log.trace_log_and_console("connect to rabbit:[%s]", text(params));

        conn = amqp_new_connection();

        int port = to!(int)(params.get("port", "5672"));
        die_on_error(sockfd =
                         amqp_open_socket(cast(char *)(params[ "host" ] ~ "\0"),
                                          port), cast(char *)("Error on opening socket (AMQP) [" ~ params[ "host" ] ~ "]"));

        amqp_set_sockfd(&conn, sockfd);
        die_on_amqp_error(amqp_login(&conn, cast(char *)(params[ "amqp_vhost" ] ~ "\0"), 0, 131072, 0,
                                     amqp_sasl_method_enum.AMQP_SASL_METHOD_PLAIN, cast(char *)(params[ "login" ] ~ "\0"),
                                     cast(char *)(params[ "credentional" ] ~ "\0")), "Logging in");
        amqp_channel_open(&conn, 1);
        die_on_amqp_error(amqp_get_rpc_reply(&conn), "Opening channel");

        queuename = amqp_cstring_bytes(cast(char[])params[ "queue" ]);
//		amqp_queue_declare(&conn, 1, queuename, 0, 0, 0, 0, amqp_empty_table);

        amqp_basic_consume_ok_t *state = amqp_basic_consume(&conn, 1, queuename, amqp_empty_bytes, 0, 0, 0, amqp_empty_table);

        if (state is null)
        {
            writeln("listner:amqp consume is fail, redeclare queue [", queuename, "]");
            amqp_queue_declare(&conn, 1, queuename, 0, 0, 0, 0, amqp_empty_table);
            //amqp_queue_bind(&conn, 1, queuename, amqp_cstring_bytes(cast(char[])"amq.direct\0"), amqp_cstring_bytes(cast(char[])"hello\0"), amqp_empty_table);

            state = amqp_basic_consume(&conn, 1, queuename, amqp_empty_bytes, 0, 0, 0, amqp_empty_table);
        }

        die_on_amqp_error(amqp_get_rpc_reply(&conn), "AMQP Consuming");
        is_success_status = true;
        return 0;
        //		}
    }

    int connect_as_req(string[ string ] params)
    {
        if (log is null)
            log = new logger("rabbitmq", "log", null);

        //		string[] need_params = ["port", "hostname", "queuename", "vhost", "login", "credentional"];
        //		if(check_params(need_params, params) == false)
        //		{
        //			fail = "не достаточно параметров, необходимо:" ~ text(need_params) ~ ", представлено:" ~ text(params);
        //			return -1;
        //		} else
        //		{
        int port = to!(int)(params[ "port" ]);

        conn = amqp_new_connection();

        die_on_error(sockfd =
                         amqp_open_socket(cast(char *)(params[ "host" ] ~ "\0"),
                                          port), cast(char *)("Error on opening socket (AMQP) [" ~ params[ "host" ] ~ "]"));

        amqp_set_sockfd(&conn, sockfd);
        die_on_amqp_error(amqp_login(&conn, cast(char *)(params[ "amqp_vhost" ] ~ "\0"), 0, 131072, 0,
                                     amqp_sasl_method_enum.AMQP_SASL_METHOD_PLAIN, cast(char *)(params[ "login" ] ~ "\0"),
                                     cast(char *)(params[ "credentional" ] ~ "\0")), "Logging in");
        amqp_channel_open(&conn, 1);
        die_on_amqp_error(amqp_get_rpc_reply(&conn), "Opening channel");

        queuename = amqp_cstring_bytes(cast(char[])params[ "queue" ]);

        amqp_queue_declare_ok_t *r = amqp_queue_declare(&conn, 1, queuename, 0, 0, 0, 0, amqp_empty_table);
        die_on_amqp_error(amqp_get_rpc_reply(&conn), "Declaring queue");
        //			amqp_queue_bind(&conn, 1, queuename, amqp_cstring_bytes(cast(char[])"amq.direct\0"), amqp_cstring_bytes(cast(char[])"hello\0"), amqp_empty_table);

        amqp_basic_consume(&conn, 1, queuename, amqp_empty_bytes, 0, 0, 0, amqp_empty_table);

        die_on_amqp_error(amqp_get_rpc_reply(&conn), "Consuming");

        // закрываем
        die_on_amqp_error(amqp_channel_close(&conn, 1, AMQP_REPLY_SUCCESS), "Closing channel");
        die_on_amqp_error(amqp_connection_close(&conn, AMQP_REPLY_SUCCESS), "Closing connection");
//		die_on_error(amqp_destroy_connection(&conn), "Ending connection");

        // открываем уже без опции consume
        conn = amqp_new_connection();

        die_on_error(sockfd =
                         amqp_open_socket(cast(char *)(params[ "host" ] ~ "\0"),
                                          port), cast(char *)("Error on opening socket (AMQP) [" ~ params[ "host" ] ~ "]"));

        amqp_set_sockfd(&conn, sockfd);
        die_on_amqp_error(amqp_login(&conn, cast(char *)(params[ "amqp_vhost" ] ~ "\0"), 0, 131072, 0,
                                     amqp_sasl_method_enum.AMQP_SASL_METHOD_PLAIN, cast(char *)(params[ "login" ] ~ "\0"),
                                     cast(char *)(params[ "credentional" ] ~ "\0")), "Logging in");
        amqp_channel_open(&conn, 1);
        die_on_amqp_error(amqp_get_rpc_reply(&conn), "Opening channel");


        is_success_status = true;
        return 0;
        //		}
    }

    // set callback function for listener ()
    void set_callback(void function(byte *txt, int size, mq_client from_client, ref ubyte[] out_data,
                                    Context context = null) _message_acceptor)
    {
        message_acceptor = _message_acceptor;
    }

    void get_count(out int cnt)
    {
        cnt = count;
    }

    // in thread listens to the queue and calls _message_acceptor
    listener_result listener()
    {
        if (log is null)
            log = new logger("rabbitmq", "log", null);

        amqp_frame_t            frame;
        int                     result;

        amqp_basic_deliver_t    *d;
        amqp_basic_properties_t *p;
        size_t                  body_target;
        size_t                  body_received;

        while (1)
        {
            amqp_maybe_release_buffers(&conn);
            result = amqp_simple_wait_frame(&conn, &frame);
            //			printf("Result %d\n", result);
            if (result < 0)
                break;

            //			printf("Frame type %d, channel %d\n", frame.frame_type, frame.channel);
            if (frame.frame_type != AMQP_FRAME_METHOD)
                continue;

            //			printf("Method %s\n", amqp_method_name(frame.payload.method.id));
            if (frame.payload.method.id != AMQP_BASIC_DELIVER_METHOD)
                continue;

            d = cast(amqp_basic_deliver_t *)frame.payload.method.decoded;
            //			printf("Delivery %u, exchange %.*s routingkey %.*s\n", d.delivery_tag, cast(int) d.exchange.len,
            //					cast(char*) d.exchange.bytes, cast(int) d.routing_key.len, cast(char*) d.routing_key.bytes);

            result = amqp_simple_wait_frame(&conn, &frame);
            if (result < 0)
                break;

            if (frame.frame_type != AMQP_FRAME_HEADER)
            {
                writeln("Expected header!");
                abort();
            }
            //			p = cast(amqp_basic_properties_t*) frame.payload.properties.decoded;
            //			if(p._flags & AMQP_BASIC_CONTENT_TYPE_FLAG)
            //			{
            //				printf("Content-type: %.*s\n", cast(int) p.content_type.len, cast(char*) p.content_type.bytes);
            //			}
            //			printf("----\n");

            body_target   = frame.payload.properties.body_size;
            body_received = 0;

            while (body_received < body_target)
            {
                result = amqp_simple_wait_frame(&conn, &frame);
                if (result < 0)
                    break;

                if (frame.frame_type != AMQP_FRAME_BODY)
                {
                    writeln("Expected body!");
                    abort();
                }

                body_received += frame.payload.body_fragment.len;
                assert(body_received <= body_target);

                //				printf("DATA:%s\r\n", frame.payload.body_fragment.bytes);

                //        amqp_dump(frame.payload.body_fragment.bytes,
                //                  frame.payload.body_fragment.len);

                try
                {
                    count++;

                    ubyte[] outbuff;
//					writeln ("len:", frame.payload.body_fragment.len);

                    message_acceptor((cast(byte *)frame.payload.body_fragment.bytes),
                                     cast(uint)(frame.payload.body_fragment.len), this, outbuff, null);

                    //					send(soc_rep, cast(char*) outbuff, cast(uint) outbuff.length, false);
//					writeln ("message_acceptor ok");
                }
                catch (Exception ex)
                {
                    log.trace("ex! user function callback, %s", ex.msg);
                }
            }

            if (body_received != body_target)
            {
                /* Can only happen when amqp_simple_wait_frame returns <= 0 */
                /* We break here to close the connection */
                break;
            }

            amqp_basic_ack(&conn, 1, d.delivery_tag, 0);
        }

        return;
    }

    int send(char *messagebody, int message_size, bool send_more)
    {
        amqp_basic_properties_t props;

        props._flags        = AMQP_BASIC_CONTENT_TYPE_FLAG | AMQP_BASIC_DELIVERY_MODE_FLAG;
        props.content_type  = amqp_cstring_bytes(cast(char[])"text/plain");
        props.delivery_mode = 2;     /* persistent delivery mode */

        amqp_bytes_t exchange = amqp_cstring_bytes((cast(char[])""));

        amqp_bytes_t _msg;
        _msg.len   = message_size - 1;
        _msg.bytes = cast(byte *)messagebody;
        die_on_error(amqp_basic_publish(&conn, 1, exchange, queuename, 0, 0, &props, _msg), cast(char *)"Publish");

        return 0;
    }

    string reciev()
    {
        return "";
    }
}

amqp_bytes_t to_amqp_string(string ss)
{
    amqp_bytes_t tt;

    tt.len   = ss.length;
    tt.bytes = cast(byte *)ss;
    return tt;
}

void die_on_amqp_error(amqp_rpc_reply_t x, string context)
{
    switch (x.reply_type)
    {
    case amqp_response_type_enum.AMQP_RESPONSE_NORMAL:
        return;

    case amqp_response_type_enum.AMQP_RESPONSE_NONE:
        writeln(context, ": missing RPC reply type!");
        break;

    case amqp_response_type_enum.AMQP_RESPONSE_LIBRARY_EXCEPTION:
        writeln(context, amqp_error_string(x.library_error));
        break;

    default:
        break;
    }

    //exit(1);
}

void die_on_error(int x, char *context)
{
    if (x < 0)
    {
        char *errstr = amqp_error_string(-x);
        printf("%s: %s\n", context, errstr);
        free(errstr);
//    exit(1);
    }
}
