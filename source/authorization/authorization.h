#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

extern const uint8_t[4] ACCESS_LIST;

uint8_t authorize_r(const char *_uri,
                    const char *_user_uri,
                    uint8_t _request_access,
                    bool _is_check_for_reload,
                    void (*_trace_acl)(const char*),
                    void (*_trace_group)(const char*),
                    void (*_trace_info)(const char*));
