module veda.bind.libwebsocketd;

import core.stdc.stdarg;
import core.sys.posix.netinet.in_;
import core.sys.posix.poll;
import core.stdc.config;

extern (C)
{
struct SSL_CTX
{}

enum CONTEXT_PORT_NO_LISTEN = -1;

// #if __x86_64__
enum _LWS_PAD_SIZE = 16;        /* Intel recommended for best performance */
enum _n            = 4 + 10;
enum LWS_PRE       = ((_n % _LWS_PAD_SIZE) ? (_n + (_LWS_PAD_SIZE - (_n % _LWS_PAD_SIZE))) : _n);

enum LWS_POSIX = 1;
//enum LWS_INLINE = inline;
//enum LWS_O_RDONLY = O_RDONLY;
//enum LWS_EXTERN = extern;
//enum lws_pollfd = pollfd;
enum LWS_SEND_BUFFER_PRE_PADDING  = LWS_PRE;
enum LWS_SEND_BUFFER_POST_PADDING = 0;

enum lws_log_levels
{
    LLL_ERR     = 1,
    LLL_WARN    = 2,
    LLL_NOTICE  = 4,
    LLL_INFO    = 8,
    LLL_DEBUG   = 16,
    LLL_PARSER  = 32,
    LLL_HEADER  = 64,
    LLL_EXT     = 128,
    LLL_CLIENT  = 256,
    LLL_LATENCY = 512,
    LLL_COUNT   = 10
}

void _lws_log(int filter, const(char) *format, ...);
void _lws_logv(int filter, const(char) *format, va_list vl);
int lwsl_timestamp(int level, char *p, int len);
void lwsl_hexdump(void *buf, size_t len);

enum lws_context_options
{
    LWS_SERVER_OPTION_REQUIRE_VALID_OPENSSL_CLIENT_CERT = 4098,
    LWS_SERVER_OPTION_SKIP_SERVER_CANONICAL_NAME        = 4,
    LWS_SERVER_OPTION_ALLOW_NON_SSL_ON_SSL_PORT         = 4104,
    LWS_SERVER_OPTION_LIBEV                             = 16,
    LWS_SERVER_OPTION_DISABLE_IPV6                      = 32,
    LWS_SERVER_OPTION_DISABLE_OS_CA_CERTS               = 64,
    LWS_SERVER_OPTION_PEER_CERT_NOT_REQUIRED            = 128,
    LWS_SERVER_OPTION_VALIDATE_UTF8                     = 256,
    LWS_SERVER_OPTION_SSL_ECDH                          = 4608,
    LWS_SERVER_OPTION_LIBUV                             = 1024,
    LWS_SERVER_OPTION_REDIRECT_HTTP_TO_HTTPS            = 6152,
    LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT                = 4096,
    LWS_SERVER_OPTION_EXPLICIT_VHOSTS                   = 8192,
    LWS_SERVER_OPTION_UNIX_SOCK                         = 16384,
    LWS_SERVER_OPTION_STS                               = 32768
}

enum lws_callback_reasons
{
    LWS_CALLBACK_ESTABLISHED                              = 0,
    LWS_CALLBACK_CLIENT_CONNECTION_ERROR                  = 1,
    LWS_CALLBACK_CLIENT_FILTER_PRE_ESTABLISH              = 2,
    LWS_CALLBACK_CLIENT_ESTABLISHED                       = 3,
    LWS_CALLBACK_CLOSED                                   = 4,
    LWS_CALLBACK_CLOSED_HTTP                              = 5,
    LWS_CALLBACK_RECEIVE                                  = 6,
    LWS_CALLBACK_RECEIVE_PONG                             = 7,
    LWS_CALLBACK_CLIENT_RECEIVE                           = 8,
    LWS_CALLBACK_CLIENT_RECEIVE_PONG                      = 9,
    LWS_CALLBACK_CLIENT_WRITEABLE                         = 10,
    LWS_CALLBACK_SERVER_WRITEABLE                         = 11,
    LWS_CALLBACK_HTTP                                     = 12,
    LWS_CALLBACK_HTTP_BODY                                = 13,
    LWS_CALLBACK_HTTP_BODY_COMPLETION                     = 14,
    LWS_CALLBACK_HTTP_FILE_COMPLETION                     = 15,
    LWS_CALLBACK_HTTP_WRITEABLE                           = 16,
    LWS_CALLBACK_FILTER_NETWORK_CONNECTION                = 17,
    LWS_CALLBACK_FILTER_HTTP_CONNECTION                   = 18,
    LWS_CALLBACK_SERVER_NEW_CLIENT_INSTANTIATED           = 19,
    LWS_CALLBACK_FILTER_PROTOCOL_CONNECTION               = 20,
    LWS_CALLBACK_OPENSSL_LOAD_EXTRA_CLIENT_VERIFY_CERTS   = 21,
    LWS_CALLBACK_OPENSSL_LOAD_EXTRA_SERVER_VERIFY_CERTS   = 22,
    LWS_CALLBACK_OPENSSL_PERFORM_CLIENT_CERT_VERIFICATION = 23,
    LWS_CALLBACK_CLIENT_APPEND_HANDSHAKE_HEADER           = 24,
    LWS_CALLBACK_CONFIRM_EXTENSION_OKAY                   = 25,
    LWS_CALLBACK_CLIENT_CONFIRM_EXTENSION_SUPPORTED       = 26,
    LWS_CALLBACK_PROTOCOL_INIT                            = 27,
    LWS_CALLBACK_PROTOCOL_DESTROY                         = 28,
    LWS_CALLBACK_WSI_CREATE                               = 29,
    LWS_CALLBACK_WSI_DESTROY                              = 30,
    LWS_CALLBACK_GET_THREAD_ID                            = 31,
    LWS_CALLBACK_ADD_POLL_FD                              = 32,
    LWS_CALLBACK_DEL_POLL_FD                              = 33,
    LWS_CALLBACK_CHANGE_MODE_POLL_FD                      = 34,
    LWS_CALLBACK_LOCK_POLL                                = 35,
    LWS_CALLBACK_UNLOCK_POLL                              = 36,
    LWS_CALLBACK_OPENSSL_CONTEXT_REQUIRES_PRIVATE_KEY     = 37,
    LWS_CALLBACK_WS_PEER_INITIATED_CLOSE                  = 38,
    LWS_CALLBACK_WS_EXT_DEFAULTS                          = 39,
    LWS_CALLBACK_CGI                                      = 40,
    LWS_CALLBACK_CGI_TERMINATED                           = 41,
    LWS_CALLBACK_CGI_STDIN_DATA                           = 42,
    LWS_CALLBACK_CGI_STDIN_COMPLETED                      = 43,
    LWS_CALLBACK_ESTABLISHED_CLIENT_HTTP                  = 44,
    LWS_CALLBACK_CLOSED_CLIENT_HTTP                       = 45,
    LWS_CALLBACK_RECEIVE_CLIENT_HTTP                      = 46,
    LWS_CALLBACK_COMPLETED_CLIENT_HTTP                    = 47,
    LWS_CALLBACK_RECEIVE_CLIENT_HTTP_READ                 = 48,
    LWS_CALLBACK_USER                                     = 1000
}

alias int lws_sockfd_type;
alias int lws_filefd_type;

struct lws_pollargs
{
    lws_sockfd_type fd;
    int             events;
    int             prev_events;
}

struct lws_plat_file_ops
{
    lws_filefd_type function(lws *, const(char) *, c_ulong *, int) open;
    int function(lws *, lws_filefd_type) close;
    c_ulong function(lws *, lws_filefd_type, c_long) seek_cur;
    int function(lws *, lws_filefd_type, c_ulong *, ubyte *, c_ulong) read;
    int function(lws *, lws_filefd_type, c_ulong *, ubyte *, c_ulong) write;
}

enum lws_extension_callback_reasons
{
    LWS_EXT_CB_SERVER_CONTEXT_CONSTRUCT      = 0,
    LWS_EXT_CB_CLIENT_CONTEXT_CONSTRUCT      = 1,
    LWS_EXT_CB_SERVER_CONTEXT_DESTRUCT       = 2,
    LWS_EXT_CB_CLIENT_CONTEXT_DESTRUCT       = 3,
    LWS_EXT_CB_CONSTRUCT                     = 4,
    LWS_EXT_CB_CLIENT_CONSTRUCT              = 5,
    LWS_EXT_CB_CHECK_OK_TO_REALLY_CLOSE      = 6,
    LWS_EXT_CB_CHECK_OK_TO_PROPOSE_EXTENSION = 7,
    LWS_EXT_CB_DESTROY                       = 8,
    LWS_EXT_CB_DESTROY_ANY_WSI_CLOSING       = 9,
    LWS_EXT_CB_ANY_WSI_ESTABLISHED           = 10,
    LWS_EXT_CB_PACKET_RX_PREPARSE            = 11,
    LWS_EXT_CB_PACKET_TX_PRESEND             = 12,
    LWS_EXT_CB_PACKET_TX_DO_SEND             = 13,
    LWS_EXT_CB_HANDSHAKE_REPLY_TX            = 14,
    LWS_EXT_CB_FLUSH_PENDING_TX              = 15,
    LWS_EXT_CB_EXTENDED_PAYLOAD_RX           = 16,
    LWS_EXT_CB_CAN_PROXY_CLIENT_CONNECTION   = 17,
    LWS_EXT_CB_1HZ                           = 18,
    LWS_EXT_CB_REQUEST_ON_WRITEABLE          = 19,
    LWS_EXT_CB_IS_WRITEABLE                  = 20,
    LWS_EXT_CB_PAYLOAD_TX                    = 21,
    LWS_EXT_CB_PAYLOAD_RX                    = 22,
    LWS_EXT_CB_OPTION_DEFAULT                = 23,
    LWS_EXT_CB_OPTION_SET                    = 24,
    LWS_EXT_CB_OPTION_CONFIRM                = 25,
    LWS_EXT_CB_NAMED_OPTION_SET              = 26
}

enum lws_write_protocol
{
    LWS_WRITE_TEXT                   = 0,
    LWS_WRITE_BINARY                 = 1,
    LWS_WRITE_CONTINUATION           = 2,
    LWS_WRITE_HTTP                   = 3,
    LWS_WRITE_PING                   = 5,
    LWS_WRITE_PONG                   = 6,
    LWS_WRITE_HTTP_FINAL             = 7,
    LWS_WRITE_HTTP_HEADERS           = 8,
    LWS_WRITE_NO_FIN                 = 64,
    LWS_WRITE_CLIENT_IGNORE_XOR_MASK = 128
}

struct lws_tokens
{
    char *token;
    int  token_len;
}

enum lws_token_indexes
{
    WSI_TOKEN_GET_URI                          = 0,
    WSI_TOKEN_POST_URI                         = 1,
    WSI_TOKEN_OPTIONS_URI                      = 2,
    WSI_TOKEN_HOST                             = 3,
    WSI_TOKEN_CONNECTION                       = 4,
    WSI_TOKEN_UPGRADE                          = 5,
    WSI_TOKEN_ORIGIN                           = 6,
    WSI_TOKEN_DRAFT                            = 7,
    WSI_TOKEN_CHALLENGE                        = 8,
    WSI_TOKEN_EXTENSIONS                       = 9,
    WSI_TOKEN_KEY1                             = 10,
    WSI_TOKEN_KEY2                             = 11,
    WSI_TOKEN_PROTOCOL                         = 12,
    WSI_TOKEN_ACCEPT                           = 13,
    WSI_TOKEN_NONCE                            = 14,
    WSI_TOKEN_HTTP                             = 15,
    WSI_TOKEN_HTTP2_SETTINGS                   = 16,
    WSI_TOKEN_HTTP_ACCEPT                      = 17,
    WSI_TOKEN_HTTP_AC_REQUEST_HEADERS          = 18,
    WSI_TOKEN_HTTP_IF_MODIFIED_SINCE           = 19,
    WSI_TOKEN_HTTP_IF_NONE_MATCH               = 20,
    WSI_TOKEN_HTTP_ACCEPT_ENCODING             = 21,
    WSI_TOKEN_HTTP_ACCEPT_LANGUAGE             = 22,
    WSI_TOKEN_HTTP_PRAGMA                      = 23,
    WSI_TOKEN_HTTP_CACHE_CONTROL               = 24,
    WSI_TOKEN_HTTP_AUTHORIZATION               = 25,
    WSI_TOKEN_HTTP_COOKIE                      = 26,
    WSI_TOKEN_HTTP_CONTENT_LENGTH              = 27,
    WSI_TOKEN_HTTP_CONTENT_TYPE                = 28,
    WSI_TOKEN_HTTP_DATE                        = 29,
    WSI_TOKEN_HTTP_RANGE                       = 30,
    WSI_TOKEN_HTTP_REFERER                     = 31,
    WSI_TOKEN_KEY                              = 32,
    WSI_TOKEN_VERSION                          = 33,
    WSI_TOKEN_SWORIGIN                         = 34,
    WSI_TOKEN_HTTP_COLON_AUTHORITY             = 35,
    WSI_TOKEN_HTTP_COLON_METHOD                = 36,
    WSI_TOKEN_HTTP_COLON_PATH                  = 37,
    WSI_TOKEN_HTTP_COLON_SCHEME                = 38,
    WSI_TOKEN_HTTP_COLON_STATUS                = 39,
    WSI_TOKEN_HTTP_ACCEPT_CHARSET              = 40,
    WSI_TOKEN_HTTP_ACCEPT_RANGES               = 41,
    WSI_TOKEN_HTTP_ACCESS_CONTROL_ALLOW_ORIGIN = 42,
    WSI_TOKEN_HTTP_AGE                         = 43,
    WSI_TOKEN_HTTP_ALLOW                       = 44,
    WSI_TOKEN_HTTP_CONTENT_DISPOSITION         = 45,
    WSI_TOKEN_HTTP_CONTENT_ENCODING            = 46,
    WSI_TOKEN_HTTP_CONTENT_LANGUAGE            = 47,
    WSI_TOKEN_HTTP_CONTENT_LOCATION            = 48,
    WSI_TOKEN_HTTP_CONTENT_RANGE               = 49,
    WSI_TOKEN_HTTP_ETAG                        = 50,
    WSI_TOKEN_HTTP_EXPECT                      = 51,
    WSI_TOKEN_HTTP_EXPIRES                     = 52,
    WSI_TOKEN_HTTP_FROM                        = 53,
    WSI_TOKEN_HTTP_IF_MATCH                    = 54,
    WSI_TOKEN_HTTP_IF_RANGE                    = 55,
    WSI_TOKEN_HTTP_IF_UNMODIFIED_SINCE         = 56,
    WSI_TOKEN_HTTP_LAST_MODIFIED               = 57,
    WSI_TOKEN_HTTP_LINK                        = 58,
    WSI_TOKEN_HTTP_LOCATION                    = 59,
    WSI_TOKEN_HTTP_MAX_FORWARDS                = 60,
    WSI_TOKEN_HTTP_PROXY_AUTHENTICATE          = 61,
    WSI_TOKEN_HTTP_PROXY_AUTHORIZATION         = 62,
    WSI_TOKEN_HTTP_REFRESH                     = 63,
    WSI_TOKEN_HTTP_RETRY_AFTER                 = 64,
    WSI_TOKEN_HTTP_SERVER                      = 65,
    WSI_TOKEN_HTTP_SET_COOKIE                  = 66,
    WSI_TOKEN_HTTP_STRICT_TRANSPORT_SECURITY   = 67,
    WSI_TOKEN_HTTP_TRANSFER_ENCODING           = 68,
    WSI_TOKEN_HTTP_USER_AGENT                  = 69,
    WSI_TOKEN_HTTP_VARY                        = 70,
    WSI_TOKEN_HTTP_VIA                         = 71,
    WSI_TOKEN_HTTP_WWW_AUTHENTICATE            = 72,
    WSI_TOKEN_PATCH_URI                        = 73,
    WSI_TOKEN_PUT_URI                          = 74,
    WSI_TOKEN_DELETE_URI                       = 75,
    WSI_TOKEN_HTTP_URI_ARGS                    = 76,
    WSI_TOKEN_PROXY                            = 77,
    WSI_TOKEN_HTTP_X_REAL_IP                   = 78,
    WSI_TOKEN_HTTP1_0                          = 79,
    _WSI_TOKEN_CLIENT_SENT_PROTOCOLS           = 80,
    _WSI_TOKEN_CLIENT_PEER_ADDRESS             = 81,
    _WSI_TOKEN_CLIENT_URI                      = 82,
    _WSI_TOKEN_CLIENT_HOST                     = 83,
    _WSI_TOKEN_CLIENT_ORIGIN                   = 84,
    _WSI_TOKEN_CLIENT_METHOD                   = 85,
    WSI_TOKEN_COUNT                            = 86,
    WSI_TOKEN_NAME_PART                        = 87,
    WSI_TOKEN_SKIPPING                         = 88,
    WSI_TOKEN_SKIPPING_SAW_CR                  = 89,
    WSI_PARSING_COMPLETE                       = 90,
    WSI_INIT_TOKEN_MUXURL                      = 91
}

struct lws_token_limits
{
    ushort[ 86 ] token_limit;
}

enum lws_close_status
{
    LWS_CLOSE_STATUS_NOSTATUS                 = 0,
    LWS_CLOSE_STATUS_NORMAL                   = 1000,
    LWS_CLOSE_STATUS_GOINGAWAY                = 1001,
    LWS_CLOSE_STATUS_PROTOCOL_ERR             = 1002,
    LWS_CLOSE_STATUS_UNACCEPTABLE_OPCODE      = 1003,
    LWS_CLOSE_STATUS_RESERVED                 = 1004,
    LWS_CLOSE_STATUS_NO_STATUS                = 1005,
    LWS_CLOSE_STATUS_ABNORMAL_CLOSE           = 1006,
    LWS_CLOSE_STATUS_INVALID_PAYLOAD          = 1007,
    LWS_CLOSE_STATUS_POLICY_VIOLATION         = 1008,
    LWS_CLOSE_STATUS_MESSAGE_TOO_LARGE        = 1009,
    LWS_CLOSE_STATUS_EXTENSION_REQUIRED       = 1010,
    LWS_CLOSE_STATUS_UNEXPECTED_CONDITION     = 1011,
    LWS_CLOSE_STATUS_TLS_FAILURE              = 1015,
    LWS_CLOSE_STATUS_NOSTATUS_CONTEXT_DESTROY = 9999
}

enum http_status
{
    HTTP_STATUS_OK                         = 200,
    HTTP_STATUS_NO_CONTENT                 = 204,
    HTTP_STATUS_MOVED_PERMANENTLY          = 301,
    HTTP_STATUS_FOUND                      = 302,
    HTTP_STATUS_SEE_OTHER                  = 303,
    HTTP_STATUS_BAD_REQUEST                = 400,
    HTTP_STATUS_UNAUTHORIZED               = 401,
    HTTP_STATUS_PAYMENT_REQUIRED           = 402,
    HTTP_STATUS_FORBIDDEN                  = 403,
    HTTP_STATUS_NOT_FOUND                  = 404,
    HTTP_STATUS_METHOD_NOT_ALLOWED         = 405,
    HTTP_STATUS_NOT_ACCEPTABLE             = 406,
    HTTP_STATUS_PROXY_AUTH_REQUIRED        = 407,
    HTTP_STATUS_REQUEST_TIMEOUT            = 408,
    HTTP_STATUS_CONFLICT                   = 409,
    HTTP_STATUS_GONE                       = 410,
    HTTP_STATUS_LENGTH_REQUIRED            = 411,
    HTTP_STATUS_PRECONDITION_FAILED        = 412,
    HTTP_STATUS_REQ_ENTITY_TOO_LARGE       = 413,
    HTTP_STATUS_REQ_URI_TOO_LONG           = 414,
    HTTP_STATUS_UNSUPPORTED_MEDIA_TYPE     = 415,
    HTTP_STATUS_REQ_RANGE_NOT_SATISFIABLE  = 416,
    HTTP_STATUS_EXPECTATION_FAILED         = 417,
    HTTP_STATUS_INTERNAL_SERVER_ERROR      = 500,
    HTTP_STATUS_NOT_IMPLEMENTED            = 501,
    HTTP_STATUS_BAD_GATEWAY                = 502,
    HTTP_STATUS_SERVICE_UNAVAILABLE        = 503,
    HTTP_STATUS_GATEWAY_TIMEOUT            = 504,
    HTTP_STATUS_HTTP_VERSION_NOT_SUPPORTED = 505
}

alias int function(lws *, lws_callback_reasons, void *, void *, c_ulong) lws_callback_function;
alias int function(lws_context *, const(lws_extension) *, lws *, lws_extension_callback_reasons, void *, void *,
                   c_ulong) lws_extension_callback_function;

struct lws_protocols
{
    const(char)*name;
    int function(lws *, lws_callback_reasons, void *, void *, size_t) callback;
    size_t per_session_data_size;
    size_t rx_buffer_size;
    uint   id;
    void   *user;
}

enum lws_ext_options_types
{
    EXTARG_NONE    = 0,
    EXTARG_DEC     = 1,
    EXTARG_OPT_DEC = 2
}

struct lws_ext_options
{
    const(char)*name;

    enum lws_ext_options_types
    {
        EXTARG_NONE    = 0,
        EXTARG_DEC     = 1,
        EXTARG_OPT_DEC = 2
    }

    lws_ext_options_types type;
}

struct lws_ext_option_arg
{
    const(char)*option_name;
    int option_index;
    const(char)*start;
    int len;
}

struct lws_extension
{
    const(char)*name;
    int function(lws_context *, const(lws_extension) *, lws *, lws_extension_callback_reasons, void *, void *, size_t) callback;
    const(char)*client_offer;
}

int lws_extension_callback_pm_deflate(lws_context *context, const(lws_extension) *ext, lws *wsi, lws_extension_callback_reasons reason, void *user,
                                      void *in_,
                                      size_t len);
int lws_set_extension_option(lws *wsi, const(char) *ext_name, const(char) *opt_name, const(char) *opt_val);

struct lws_protocol_vhost_options
{
    const(lws_protocol_vhost_options)*next;
    const(lws_protocol_vhost_options)*options;
    const(char)*name;
    const(char)*value;
}

struct lws_http_mount
{
    lws_http_mount *mount_next;
    const(char)*mountpoint;
    const(char)*origin;
    const(char)*def;
    const(lws_protocol_vhost_options)*cgienv;
    int   cgi_timeout;
    int   cache_max_age;
    uint  cache_reusable;
    uint  cache_revalidate;
    uint  cache_intermediaries;
    ubyte origin_protocol;
    ubyte mountpoint_len;
}

struct lws_context_creation_info
{
    int port;
    const(char)*iface;
    const(lws_protocols)*protocols;
    const(lws_extension)*extensions;
    const(lws_token_limits)*token_limits;
    const(char)*ssl_private_key_password;
    const(char)*ssl_cert_filepath;
    const(char)*ssl_private_key_filepath;
    const(char)*ssl_ca_filepath;
    const(char)*ssl_cipher_list;
    const(char)*http_proxy_address;
    uint    http_proxy_port;
    int     gid;
    int     uid;
    uint    options;
    void    *user;
    int     ka_time;
    int     ka_probes;
    int     ka_interval;
    SSL_CTX *provided_client_ssl_ctx;
    short   max_http_header_data;
    short   max_http_header_pool;
    uint    count_threads;
    uint    fd_limit_per_thread;
    uint    timeout_secs;
    const(char)*ecdh_curve;
    const(char)*vhost_name;
    const(char *)*plugin_dirs;
    const(lws_protocol_vhost_options)*pvo;
    int keepalive_timeout;
    const(char)*log_filepath;
    const(lws_http_mount)*mounts;
    const(char)*server_string;
    void *[ 8 ] _unused;
}

struct lws_client_connect_info
{
    lws_context *context;
    const(char)*address;
    int         port;
    int         ssl_connection;
    const(char)*path;
    const(char)*host;
    const(char)*origin;
    const(char)*protocol;
    int       ietf_version_or_minus_one;
    void      *userdata;
    const(lws_extension)*client_exts;
    const(char)*method;
    lws       *parent_wsi;
    const(char)*uri_replace_from;
    const(char)*uri_replace_to;
    lws_vhost *vhost;
    void *[ 4 ] _unused;
}

enum _Anonymous_0
{
    LWSMPRO_HTTP        = 0,
    LWSMPRO_HTTPS       = 1,
    LWSMPRO_FILE        = 2,
    LWSMPRO_CGI         = 3,
    LWSMPRO_REDIR_HTTP  = 4,
    LWSMPRO_REDIR_HTTPS = 5
}

int lws_json_dump_vhost(const(lws_vhost) *vh, char *buf, int len);
int lws_json_dump_context(const(lws_context) *context, char *buf, int len);
void lws_set_log_level(int level, void function(int, const(char)*) log_emit_function);
void lwsl_emit_syslog(int level, const(char) *line);
lws_context *lws_create_context(lws_context_creation_info *info);
lws_vhost *lws_create_vhost(lws_context *context, lws_context_creation_info *info);
lws_vhost *lws_vhost_get(lws *wsi);
const(lws_protocols) *lws_protocol_get(lws *wsi);
void *lws_protocol_vh_priv_zalloc(lws_vhost *vhost, const(lws_protocols) *prot, int size);
void *lws_protocol_vh_priv_get(lws_vhost *vhost, const(lws_protocols) *prot);
int lws_finalize_startup(lws_context *context);
int lws_set_proxy(lws_vhost *vhost, const(char) *proxy);
void lws_context_destroy(lws_context *context);
int lws_service(lws_context *context, int timeout_ms);
int lws_service_tsi(lws_context *context, int timeout_ms, int tsi);
void lws_cancel_service_pt(lws *wsi);
void lws_cancel_service(lws_context *context);
int lws_interface_to_sa(int ipv6, const(char) *ifname, sockaddr_in *addr, size_t addrlen);
const(ubyte) *lws_token_to_string(lws_token_indexes token);
int lws_add_http_header_by_name(lws *wsi, const(ubyte) *name, const(ubyte) *value, int length, ubyte **p, ubyte *end);
int lws_finalize_http_header(lws *wsi, ubyte **p, ubyte *end);
int lws_add_http_header_by_token(lws *wsi, lws_token_indexes token, const(ubyte) *value, int length, ubyte **p, ubyte *end);
int lws_add_http_header_content_length(lws *wsi, c_ulong content_length, ubyte **p, ubyte *end);
int lws_add_http_header_status(lws *wsi, uint code, ubyte **p, ubyte *end);
int lws_http_redirect(lws *wsi, int code, const(ubyte) *loc, int len, ubyte **p, ubyte *end);
int lws_http_transaction_completed(lws *wsi);
int lws_service_fd(lws_context *context, pollfd *pollfd);
int lws_service_fd_tsi(lws_context *context, pollfd *pollfd, int tsi);
void *lws_context_user(lws_context *context);
void *lws_wsi_user(lws *wsi);

enum pending_timeout
{
    NO_PENDING_TIMEOUT                                  = 0,
    PENDING_TIMEOUT_AWAITING_PROXY_RESPONSE             = 1,
    PENDING_TIMEOUT_AWAITING_CONNECT_RESPONSE           = 2,
    PENDING_TIMEOUT_ESTABLISH_WITH_SERVER               = 3,
    PENDING_TIMEOUT_AWAITING_SERVER_RESPONSE            = 4,
    PENDING_TIMEOUT_AWAITING_PING                       = 5,
    PENDING_TIMEOUT_CLOSE_ACK                           = 6,
    PENDING_TIMEOUT_AWAITING_EXTENSION_CONNECT_RESPONSE = 7,
    PENDING_TIMEOUT_SENT_CLIENT_HANDSHAKE               = 8,
    PENDING_TIMEOUT_SSL_ACCEPT                          = 9,
    PENDING_TIMEOUT_HTTP_CONTENT                        = 10,
    PENDING_TIMEOUT_AWAITING_CLIENT_HS_SEND             = 11,
    PENDING_FLUSH_STORED_SEND_BEFORE_CLOSE              = 12,
    PENDING_TIMEOUT_SHUTDOWN_FLUSH                      = 13,
    PENDING_TIMEOUT_CGI                                 = 14,
    PENDING_TIMEOUT_HTTP_KEEPALIVE_IDLE                 = 15
}

void lws_set_timeout(lws *wsi, pending_timeout reason, int secs);
int lws_write(lws *wsi, ubyte *buf, size_t len, lws_write_protocol protocol);
void lws_close_reason(lws *wsi, lws_close_status status, ubyte *buf, size_t len);
int lws_serve_http_file(lws *wsi, const(char) *file, const(char) *content_type, const(char) *other_headers, int other_headers_len);
int lws_serve_http_file_fragment(lws *wsi);
int lws_return_http_status(lws *wsi, uint code, const(char) *html_body);
const(lws_protocols) *lws_get_protocol(lws *wsi);
int lws_callback_on_writable(lws *wsi);
int lws_callback_on_writable_all_protocol(const(lws_context) *context, const(lws_protocols) *protocol);
int lws_callback_on_writable_all_protocol_vhost(const(lws_vhost) *vhost, const(lws_protocols) *protocol);
int lws_callback_all_protocol(lws_context *context, const(lws_protocols) *protocol, int reason);
int lws_callback_all_protocol_vhost(lws_vhost *vh, const(lws_protocols) *protocol, int reason);
int lws_get_socket_fd(lws *wsi);
int lws_is_final_fragment(lws *wsi);
ubyte lws_get_reserved_bits(lws *wsi);
int lws_rx_flow_control(lws *wsi, int enable);
void lws_rx_flow_allow_all_protocol(const(lws_context) *context, const(lws_protocols) *protocol);
size_t lws_remaining_packet_payload(lws *wsi);
size_t lws_get_peer_write_allowance(lws *wsi);
lws *lws_client_connect(lws_context *clients, const(char) *address, int port, int ssl_connection, const(char) *path, const(char) *host,
                        const(char) *origin, const(char) *protocol,
                        int ietf_version_or_minus_one);
lws *lws_client_connect_extended(lws_context *clients, const(char) *address, int port, int ssl_connection, const(char) *path, const(char) *host,
                                 const(char) *origin, const(char) *protocol, int ietf_version_or_minus_one,
                                 void *userdata);
lws *lws_client_connect_via_info(lws_client_connect_info *ccinfo);
lws *lws_adopt_socket(lws_context *context, lws_sockfd_type accept_fd);
lws *lws_adopt_socket_readbuf(lws_context *context, lws_sockfd_type accept_fd, const(char) *readbuf, size_t len);
const(char) *lws_canonical_hostname(lws_context *context);
void lws_get_peer_addresses(lws *wsi, lws_sockfd_type fd, char *name, int name_len, char *rip, int rip_len);
int lws_get_random(lws_context *context, void *buf, int len);
int lws_daemonize(const(char) *_lock_path);
int lws_send_pipe_choked(lws *wsi);
int lws_partial_buffered(lws *wsi);
int lws_frame_is_binary(lws *wsi);
int lws_is_ssl(lws *wsi);
int lws_is_cgi(lws *wsi);
ubyte *lws_SHA1(const(ubyte) *d, size_t n, ubyte *md);
int lws_b64_encode_string(const(char) *in_, int in_len, char *out_, int out_size);
int lws_b64_decode_string(const(char) *in_, char *out_, int out_size);
const(char) *lws_get_library_version();
int lws_parse_uri(char *p, const(char *) *prot, const(char *) *ads, int *port, const(char *) *path);
int lws_hdr_total_length(lws *wsi, lws_token_indexes h);
int lws_hdr_fragment_length(lws *wsi, lws_token_indexes h, int frag_idx);
int lws_hdr_copy(lws *wsi, char *dest, int len, lws_token_indexes h);
int lws_hdr_copy_fragment(lws *wsi, char *dest, int len, lws_token_indexes h, int frag_idx);
lws_plat_file_ops *lws_get_fops(lws_context *context);
lws_context *lws_get_context(const(lws) *wsi);
int lws_get_count_threads(lws_context *context);
lws *lws_get_parent(const(lws) *wsi);
lws *lws_get_child(const(lws) *wsi);
int lws_http_client_read(lws *wsi, char **buf, int *len);
lws_filefd_type lws_plat_file_open(lws *wsi, const(char) *filename, c_ulong *filelen, int flags);
int lws_plat_file_close(lws *wsi, lws_filefd_type fd);
c_ulong lws_plat_file_seek_cur(lws *wsi, lws_filefd_type fd, c_long offset);
int lws_plat_file_read(lws *wsi, lws_filefd_type fd, c_ulong *amount, ubyte *buf, c_ulong len);
int lws_plat_file_write(lws *wsi, lws_filefd_type fd, c_ulong *amount, ubyte *buf, c_ulong len);
int lws_read(lws *wsi, ubyte *buf, size_t len);
//const(lws_extension)* lws_get_internal_extensions (...);
int lws_ext_parse_options(const(lws_extension) *ext, lws *wsi, void *ext_user, const(lws_ext_options) *opts, const(char) *o, int len);
void lws_set_allocator(void *function(void *, size_t) realloc);
struct lws;
struct lws_context;
struct lws_vhost;
}
