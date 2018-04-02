#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define ROLE_OBJECT 1

#define ROLE_SUBJECT 0

#define TRACE_ACL 0

#define TRACE_GROUP 1

#define TRACE_INFO 2

//extern const uint8_t[4] ACCESS_LIST;

//extern const const str*[9] ACCESS_LIST_PREDICATES;

uint8_t authorize_r(const char *_uri,
                  const char *_user_uri,
                  uint8_t _request_access,
                  bool _is_check_for_reload);

const char *get_trace(const char *_uri,
                         const char *_user_uri,
                         uint8_t _request_access,
                         uint8_t trace_mode,
                         bool _is_check_for_reload);
