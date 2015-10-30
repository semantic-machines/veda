/**
 * обвязка к rabbit mq
 */

module bind.librabbitmq_headers;

private import std.c.string;

public static short AMQP_PROTOCOL_VERSION_MAJOR    = 0;
public static short AMQP_PROTOCOL_VERSION_MINOR    = 9;
public static short AMQP_PROTOCOL_VERSION_REVISION = 1;
public static short AMQP_PROTOCOL_PORT             = 5672;
public static short AMQP_FRAME_METHOD              = 1;
public static short AMQP_FRAME_HEADER              = 2;
public static short AMQP_FRAME_BODY                = 3;
public static short AMQP_FRAME_HEARTBEAT           = 8;
public static short AMQP_FRAME_MIN_SIZE            = 4096;
public static short AMQP_FRAME_END                 = 206;
public static short AMQP_REPLY_SUCCESS             = 200;
public static short AMQP_CONTENT_TOO_LARGE         = 311;
public static short AMQP_NO_ROUTE                  = 312;
public static short AMQP_NO_CONSUMERS              = 313;
public static short AMQP_ACCESS_REFUSED            = 403;
public static short AMQP_NOT_FOUND                 = 404;
public static short AMQP_RESOURCE_LOCKED           = 405;
public static short AMQP_PRECONDITION_FAILED       = 406;
public static short AMQP_CONNECTION_FORCED         = 320;
public static short AMQP_INVALID_PATH              = 402;
public static short AMQP_FRAME_ERROR               = 501;
public static short AMQP_SYNTAX_ERROR              = 502;
public static short AMQP_COMMAND_INVALID           = 503;
public static short AMQP_CHANNEL_ERROR             = 504;
public static short AMQP_UNEXPECTED_FRAME          = 505;
public static short AMQP_RESOURCE_ERROR            = 506;
public static short AMQP_NOT_ALLOWED               = 530;
public static short AMQP_NOT_IMPLEMENTED           = 540;
public static short AMQP_INTERNAL_ERROR            = 541;

public static int   AMQP_BASIC_CLASS                 = (0x003C); /* 60 */
public static int   AMQP_BASIC_CONTENT_TYPE_FLAG     = (1 << 15);
public static int   AMQP_BASIC_CONTENT_ENCODING_FLAG = (1 << 14);
public static int   AMQP_BASIC_HEADERS_FLAG          = (1 << 13);
public static int   AMQP_BASIC_DELIVERY_MODE_FLAG    = (1 << 12);
public static int   AMQP_BASIC_PRIORITY_FLAG         = (1 << 11);
public static int   AMQP_BASIC_CORRELATION_ID_FLAG   = (1 << 10);
public static int   AMQP_BASIC_REPLY_TO_FLAG         = (1 << 9);
public static int   AMQP_BASIC_EXPIRATION_FLAG       = (1 << 8);
public static int   AMQP_BASIC_MESSAGE_ID_FLAG       = (1 << 7);
public static int   AMQP_BASIC_TIMESTAMP_FLAG        = (1 << 6);
public static int   AMQP_BASIC_TYPE_FLAG             = (1 << 5);
public static int   AMQP_BASIC_USER_ID_FLAG          = (1 << 4);
public static int   AMQP_BASIC_APP_ID_FLAG           = (1 << 3);
public static int   AMQP_BASIC_CLUSTER_ID_FLAG       = (1 << 2);

alias byte          int8_t;
alias ubyte         uint8_t;
alias short         int16_t;
alias ushort        uint16_t;
alias int           int32_t;
alias uint          uint32_t;
alias long          int64_t;
alias ulong         uint64_t;

alias int           amqp_boolean_t;
alias uint          amqp_method_number_t;
alias uint          amqp_flags_t;
alias ushort        amqp_channel_t;

struct amqp_bytes_t
{
    size_t len;
    void   *bytes;
};

struct amqp_pool_blocklist_t
{
    int  num_blocks;
    void **blocklist;
};

struct amqp_pool_t
{
    size_t                pagesize;

    amqp_pool_blocklist_t pages;
    amqp_pool_blocklist_t large_blocks;

    int                   next_page;
    char                  *alloc_block;
    size_t                alloc_used;
};

enum amqp_connection_state_enum
{
    CONNECTION_STATE_IDLE = 0,
    CONNECTION_STATE_INITIAL,
    CONNECTION_STATE_HEADER,
    CONNECTION_STATE_BODY
};

struct amqp_link_t
{
    amqp_link_t *next;
    void        *data;
};

enum amqp_response_type_enum
{
    AMQP_RESPONSE_NONE = 0,
    AMQP_RESPONSE_NORMAL,
    AMQP_RESPONSE_LIBRARY_EXCEPTION,
    AMQP_RESPONSE_SERVER_EXCEPTION
};

struct amqp_method_t
{
    amqp_method_number_t id;
    void                 *decoded;
};

struct amqp_rpc_reply_t
{
    amqp_response_type_enum reply_type;
    amqp_method_t           reply;
    int                     library_error; /* if AMQP_RESPONSE_LIBRARY_EXCEPTION, then 0 here means socket EOF */
};

struct amqp_connection_state_t
{
    amqp_pool_t                frame_pool;
    amqp_pool_t                decoding_pool;

    amqp_connection_state_enum state;

    int                        channel_max;
    int                        frame_max;
    int                        heartbeat;
    amqp_bytes_t               inbound_buffer;

    size_t                     inbound_offset;
    size_t                     target_size;

    amqp_bytes_t               outbound_buffer;

    int                        sockfd;

    amqp_bytes_t               sock_inbound_buffer;
    size_t                     sock_inbound_offset;
    size_t                     sock_inbound_limit;

    amqp_link_t                *first_queued_frame;
    amqp_link_t                *last_queued_frame;

    amqp_rpc_reply_t           most_recent_api_result;
};

enum amqp_sasl_method_enum
{
    AMQP_SASL_METHOD_PLAIN = 0
};

struct amqp_channel_open_ok_t
{
    amqp_bytes_t channel_id;
};

struct amqp_basic_consume_ok_t
{
    amqp_bytes_t consumer_tag;
};

union u1
{
    amqp_boolean_t boolean;
    int8_t         i8;
    uint8_t        u8;
    int16_t        i16;
    uint16_t       u16;
    int32_t        i32;
    uint32_t       u32;
    int64_t        i64;
    uint64_t       u64;
    float          f32;
    double         f64;
    amqp_decimal_t decimal;
    amqp_bytes_t   bytes;
    amqp_table_t   table;
    amqp_array_t   array;
};

struct amqp_field_value_t
{
    uint8_t kind;
    u1      value;
};

struct amqp_table_entry_t
{
    amqp_bytes_t       key;
    amqp_field_value_t value;
};

struct amqp_table_t
{
    int                num_entries;
    amqp_table_entry_t *entries;
};

struct amqp_decimal_t
{
    int      decimals;
    uint32_t value;
};

struct amqp_array_t
{
    int                num_entries;
    amqp_field_value_t *entries;
};

amqp_bytes_t amqp_empty_bytes = { 0, null };
amqp_table_t amqp_empty_table = { 0, null };
amqp_array_t amqp_empty_array = { 0, null };

amqp_bytes_t amqp_cstring_bytes(char[] cstr)
{
    amqp_bytes_t result;

    result.len   = cstr.length;
    result.bytes = cast(void *)cstr;
    return result;
}

struct struct_protocol_header
{
    uint8_t transport_high;
    uint8_t transport_low;
    uint8_t protocol_version_major;
    uint8_t protocol_version_minor;
}

struct struct_properties
{
    uint16_t     class_id;
    uint64_t     body_size;
    void         *decoded;
    amqp_bytes_t raw;
}

union union_payload
{
    amqp_method_t          method;
    struct_properties      properties;
    amqp_bytes_t           body_fragment;
    struct_protocol_header protocol_header;
}

struct amqp_frame_t
{
    uint8_t        frame_type; /* 0 means no event */
    amqp_channel_t channel;
    union_payload  payload;
}

struct amqp_basic_deliver_t
{
    amqp_bytes_t   consumer_tag;
    uint64_t       delivery_tag;
    amqp_boolean_t redelivered;
    amqp_bytes_t   exchange;
    amqp_bytes_t   routing_key;
}

struct amqp_basic_properties_t
{
    amqp_flags_t _flags;
    amqp_bytes_t content_type;
    amqp_bytes_t content_encoding;
    amqp_table_t headers;
    uint8_t      delivery_mode;
    uint8_t      priority;
    amqp_bytes_t correlation_id;
    amqp_bytes_t reply_to;
    amqp_bytes_t expiration;
    amqp_bytes_t message_id;
    uint64_t     timestamp;
    amqp_bytes_t type;
    amqp_bytes_t user_id;
    amqp_bytes_t app_id;
    amqp_bytes_t cluster_id;
};

amqp_method_number_t AMQP_BASIC_DELIVER_METHOD = 0x003C003C; /* 60, 60; 3932220 */

extern (C)
amqp_connection_state_t amqp_new_connection();

extern (C)
int amqp_open_socket(char *hostname, int portnumber);

extern (C)
int amqp_destroy_connection(amqp_connection_state_t *state);

extern (C)
int amqp_socket_error();

extern (C)
char *amqp_os_error_string(int err);

extern (C)
void amqp_set_sockfd(amqp_connection_state_t *state, int sockfd);

extern (C)
amqp_rpc_reply_t amqp_login(amqp_connection_state_t *state, char *vhost, int channel_max, int frame_max, int heartbeat,
                            amqp_sasl_method_enum sasl_method, ...);

extern (C)
amqp_channel_open_ok_t * amqp_channel_open(amqp_connection_state_t * state, amqp_channel_t channel);

extern (C)
amqp_rpc_reply_t amqp_get_rpc_reply(amqp_connection_state_t *state);

extern (C)
amqp_basic_consume_ok_t * amqp_basic_consume(amqp_connection_state_t * state, amqp_channel_t channel, amqp_bytes_t queue,
                                             amqp_bytes_t consumer_tag, amqp_boolean_t no_local, amqp_boolean_t no_ack,
                                             amqp_boolean_t exclusive,
                                             amqp_table_t arguments);


struct amqp_queue_bind_t
{
    uint16_t       ticket;
    amqp_bytes_t   queue;
    amqp_bytes_t   exchange;
    amqp_bytes_t   routing_key;
    amqp_boolean_t nowait;
    amqp_table_t   arguments;
};

struct amqp_queue_bind_ok_t
{
    char dummy; /* Dummy field to avoid empty struct */
};


extern (C)
amqp_queue_bind_ok_t * amqp_queue_bind(amqp_connection_state_t * state, amqp_channel_t channel, amqp_bytes_t queue, amqp_bytes_t exchange,
                                       amqp_bytes_t routing_key, amqp_table_t arguments);

struct amqp_queue_declare_t
{
    uint16_t       ticket;
    amqp_bytes_t   queue;
    amqp_boolean_t passive;
    amqp_boolean_t durable;
    amqp_boolean_t exclusive;
    amqp_boolean_t auto_delete;
    amqp_boolean_t nowait;
    amqp_table_t   arguments;
};

struct amqp_queue_declare_ok_t
{
    amqp_bytes_t queue;
    uint32_t     message_count;
    uint32_t     consumer_count;
};

extern (C)
amqp_queue_declare_ok_t * amqp_queue_declare(amqp_connection_state_t * state, amqp_channel_t channel, amqp_bytes_t queue,
                                             amqp_boolean_t passive, amqp_boolean_t durable, amqp_boolean_t exclusive,
                                             amqp_boolean_t auto_delete, amqp_table_t arguments);


extern (C)
void amqp_maybe_release_buffers(amqp_connection_state_t *state);

extern (C)
int amqp_simple_wait_frame(amqp_connection_state_t *state, amqp_frame_t *decoded_frame);

extern (C)
char *amqp_method_name(amqp_method_number_t methodNumber);

extern (C)
int amqp_basic_ack(amqp_connection_state_t *state, amqp_channel_t channel, uint64_t delivery_tag, amqp_boolean_t multiple);

extern (C)
amqp_rpc_reply_t amqp_channel_close(amqp_connection_state_t *state, amqp_channel_t channel, int code);

extern (C)
amqp_rpc_reply_t amqp_connection_close(amqp_connection_state_t *state, int code);

// --- utils ----
//extern(C)
//	void die_on_error(int x, immutable char* context);

//extern(C)
//	void die_on_amqp_error(amqp_rpc_reply_t x, immutable char* context);

//extern(C)
//	extern void amqp_dump(void* buffer, size_t len);

//extern(C)
//	extern uint64_t now_microseconds();

//extern(C)
//	extern void microsleep(int usec);

extern (C) char *amqp_error_string(int err);


struct amqp_basic_publish_t
{
    uint16_t       ticket;
    amqp_bytes_t   exchange;
    amqp_bytes_t   routing_key;
    amqp_boolean_t mandatory;
    amqp_boolean_t immediate;
};

extern (C)
int
amqp_basic_publish(amqp_connection_state_t *state, amqp_channel_t channel,
                   amqp_bytes_t exchange, amqp_bytes_t routing_key,
                   amqp_boolean_t mandatory, amqp_boolean_t immediate,
                   amqp_basic_properties_t *properties,
                   amqp_bytes_t _body);


