/**
 * обвязка к libraptor2
 */
module veda.bind.libraptor_header;


// //////////////////////////  call D from C //////////////////////////////////////////

struct raptor_world
{
}

struct raptor_parser
{
}

struct raptor_serializer
{
}

struct raptor_uri
{
}

/* I/O Stream utility functions */

/**
 * raptor_escaped_write_bitflags:
 * @RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_BF   : Allow \b \f,
 * @RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_TNRU : ALlow \t \n \r \u
 * @RAPTOR_ESCAPED_WRITE_BITFLAG_UTF8            : Allow UTF-8 for printable U *
 * @RAPTOR_ESCAPED_WRITE_BITFLAG_SPARQL_URI_ESCAPES: Must escape #x00-#x20<>\"{}|^` in URIs
 * @RAPTOR_ESCAPED_WRITE_NTRIPLES_LITERAL: N-Triples literal
 * @RAPTOR_ESCAPED_WRITE_NTRIPLES_URI: N-Triples URI
 * @RAPTOR_ESCAPED_WRITE_SPARQL_LITERAL: SPARQL literal: allows raw UTF8 for printable literals
 * @RAPTOR_ESCAPED_WRITE_SPARQL_LONG_LITERAL: SPARQL long literal: no BS-escapes allowed
 * @RAPTOR_ESCAPED_WRITE_SPARQL_URI: SPARQL uri: have to escape certain characters
 * @RAPTOR_ESCAPED_WRITE_TURTLE_URI: Turtle 2013 URIs (like SPARQL)
 * @RAPTOR_ESCAPED_WRITE_TURTLE_LITERAL: Turtle 2013 literals (like SPARQL)
 * @RAPTOR_ESCAPED_WRITE_TURTLE_LONG_LITERAL: Turtle 2013 long literals (like SPARQL)
 * @RAPTOR_ESCAPED_WRITE_JSON_LITERAL: JSON literals: \b \f \t \r \n and \u \U
 *
 * Bit flags for raptor_string_escaped_write() and friends.
 */
enum
{
    RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_BF      = 1,
    RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_TNRU    = 2,
    RAPTOR_ESCAPED_WRITE_BITFLAG_UTF8               = 4,
    RAPTOR_ESCAPED_WRITE_BITFLAG_SPARQL_URI_ESCAPES = 8,

    /* N-Triples - favour writing \u, \U over UTF8 */
    RAPTOR_ESCAPED_WRITE_NTRIPLES_LITERAL           = RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_TNRU |
                                                      RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_BF,
    RAPTOR_ESCAPED_WRITE_NTRIPLES_URI               = RAPTOR_ESCAPED_WRITE_BITFLAG_SPARQL_URI_ESCAPES,

    /* SPARQL literal: allows raw UTF8 for printable literals */
    RAPTOR_ESCAPED_WRITE_SPARQL_LITERAL             = RAPTOR_ESCAPED_WRITE_BITFLAG_UTF8,

    /* SPARQL long literal: no BS-escapes allowed */
    RAPTOR_ESCAPED_WRITE_SPARQL_LONG_LITERAL        = RAPTOR_ESCAPED_WRITE_BITFLAG_UTF8,

    /* SPARQL uri: have to escape certain characters */
    RAPTOR_ESCAPED_WRITE_SPARQL_URI                 = RAPTOR_ESCAPED_WRITE_BITFLAG_UTF8 | RAPTOR_ESCAPED_WRITE_BITFLAG_SPARQL_URI_ESCAPES,

    /* Turtle (2013) escapes are like SPARQL */
    RAPTOR_ESCAPED_WRITE_TURTLE_URI                 = RAPTOR_ESCAPED_WRITE_SPARQL_URI,
    RAPTOR_ESCAPED_WRITE_TURTLE_LITERAL             = RAPTOR_ESCAPED_WRITE_SPARQL_LITERAL,
    RAPTOR_ESCAPED_WRITE_TURTLE_LONG_LITERAL        = RAPTOR_ESCAPED_WRITE_SPARQL_LONG_LITERAL,

    /* JSON literals: \b \f \t \r \n and \u \U */
    RAPTOR_ESCAPED_WRITE_JSON_LITERAL               = RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_TNRU |
                                                      RAPTOR_ESCAPED_WRITE_BITFLAG_BS_ESCAPES_BF
};


/**
 * raptor_term_type:
 * @RAPTOR_TERM_TYPE_URI: RDF URI
 * @RAPTOR_TERM_TYPE_LITERAL: RDF literal
 * @RAPTOR_TERM_TYPE_BLANK: RDF blank node
 * @RAPTOR_TERM_TYPE_UNKNOWN: Internal
 *
 * Type of term in a #raptor_statement
 *
 * Node type 3 is unused but exists to preserve numeric compatibility
 * with librdf_node_type values.
 */
enum raptor_term_type
{
    RAPTOR_TERM_TYPE_UNKNOWN = 0,
    RAPTOR_TERM_TYPE_URI     = 1,
    RAPTOR_TERM_TYPE_LITERAL = 2,
    /* unused type 3 */
    RAPTOR_TERM_TYPE_BLANK   = 4
}

/**
 * raptor_term:
 * @world: world
 * @usage: usage reference count (if >0)
 * @type: term type
 * @value: term values per type
 *
 * An RDF statement term
 *
 */
struct raptor_term
{
    raptor_world      *world;

    int               usage;

    raptor_term_type  type;

    raptor_term_value value;
}

/**
 * raptor_statement:
 * @world: world pointer
 * @usage: usage count
 * @subject: statement subject
 * @predicate: statement predicate
 * @object: statement object
 * @graph: statement graph name (or NULL if not present)
 *
 * An RDF triple with optional graph name (quad)
 *
 * See #raptor_term for a description of how the fields may be used.
 * As returned by a parser statement_handler.
 */
struct raptor_statement
{
    raptor_world *world;
    int          usage;
    raptor_term  *subject;
    raptor_term  *predicate;
    raptor_term  *object;
    raptor_term  *graph;
}

struct raptor_namespace_stack
{
}

struct raptor_namespace_s
{
    /* next down the stack, NULL at bottom */
    raptor_namespace_s     *next;

    raptor_namespace_stack *nstack;

    /* NULL means is the default namespace */
    char                   *prefix;
    /* needed to safely compare prefixed-names */
    uint                   prefix_length;
    /* URI of namespace or NULL for default */
    raptor_uri             *uri;
    /* parsing depth that this ns was added.  It will
     * be deleted when the parser leaves this depth
     */
    int depth;
    /* Non 0 if is xml: prefixed name */
    int is_xml;
    /* Non 0 if is RDF M&S Namespace */
    int is_rdf_ms;
    /* Non 0 if is RDF Schema Namespace */
    int is_rdf_schema;
}

alias raptor_namespace_s raptor_namespace;

/**
 * raptor_term_value:
 * @uri: uri value when term type is #RAPTOR_TERM_TYPE_URI
 * @literal: literal value when term type is #RAPTOR_TERM_TYPE_LITERAL
 * @blank: blank value when term type is #RAPTOR_TERM_TYPE_BLANK
 *
 * Term value - this typedef exists solely for use in #raptor_term
 *
 **/
union raptor_term_value
{
    raptor_uri                *uri;

    raptor_term_literal_value literal;

    raptor_term_blank_value   blank;
}

/**
 * raptor_term_literal_value:
 * @string: literal string
 * @string_len: length of string
 * @datatype: datatype URI (or NULL)
 * @language: literal language (or NULL)
 * @language_len: length of language
 *
 * Literal term value - this typedef exists solely for use in #raptor_term
 *
 * Either @datatype or @language may be non-NULL but not both.
 */
struct raptor_term_literal_value
{
    char       *_string;
    uint       string_len;

    raptor_uri *datatype;

    char       *language;
    char       language_len;
}

/**
 * raptor_term_blank_value:
 * @string: literal string
 * @string_len: length of string
 *
 * Blank term value - this typedef exists solely for use in #raptor_term
 *
 */
struct raptor_term_blank_value
{
    char *_string;
    uint string_len;
}

struct raptor_iostream_s
{
}

alias raptor_iostream_s raptor_iostream;


extern (C)raptor_parser * raptor_new_parser(raptor_world * world, immutable(char *) name);

/**
 * raptor_statement_handler:
 * @user_data: user data
 * @statement: statement to report
 *
 * Statement (triple) reporting handler function.
 *
 * This handler function set with
 * raptor_parser_set_statement_handler() on a parser receives
 * statements as the parsing proceeds. The @statement argument to the
 * handler is shared and must be copied by the caller with
 * raptor_statement_copy().
 */
//void function(void *user_data, raptor_statement *statement) raptor_statement_handler;


extern (C) void raptor_parser_set_statement_handler(raptor_parser *parser, void *user_data, void function(void *user_data,
                                                                                                          raptor_statement *statement));
extern (C) void raptor_parser_set_namespace_handler(raptor_parser *parser, void *user_data, void function(void *user_data,
                                                                                                          raptor_namespace *nspace));

extern (C)raptor_world * raptor_new_world_internal(uint version_decimal = 20015);
extern (C) char *raptor_uri_filename_to_uri_string(immutable(char *) filename);
extern (C)raptor_uri * raptor_new_uri(raptor_world * world, char *uri_string);
extern (C)raptor_uri * raptor_uri_copy(raptor_uri * uri);
extern (C) int raptor_parser_parse_file(raptor_parser *rdf_parser, raptor_uri *uri, raptor_uri *base_uri);
extern (C) char *raptor_term_to_string(raptor_term * term);
extern (C) char *raptor_uri_as_string(raptor_uri * uri);
extern (C)raptor_iostream * raptor_new_iostream_to_string(raptor_world * world, void **string_p, size_t * length_p, void *function(
                                                                                                                                   size_t size));
extern (C) int raptor_term_escaped_write(raptor_term *term, uint flags, raptor_iostream *iostr);
extern (C) void raptor_free_iostream(raptor_iostream *iostr);
extern (C) void raptor_free_parser(raptor_parser *parser);
extern (C) void raptor_free_uri(raptor_uri *uri);
extern (C) void raptor_free_memory(void *ptr);
extern (C) void raptor_free_world(raptor_world *world);


/* Serializer Class */
extern (C)raptor_serializer * raptor_new_serializer(raptor_world * world, const char *name);
extern (C) void raptor_free_serializer(raptor_serializer *rdf_serializer);

/* methods */
extern (C) int raptor_serializer_start_to_iostream(raptor_serializer *rdf_serializer, raptor_uri *uri, raptor_iostream *iostream);
extern (C) int raptor_serializer_start_to_filename(raptor_serializer *rdf_serializer, immutable(char *)filename);
extern (C) int raptor_serializer_start_to_string(raptor_serializer *rdf_serializer, raptor_uri *uri, void **string_p, size_t *length_p);
extern (C) int raptor_serializer_serialize_statement(raptor_serializer *rdf_serializer, raptor_statement *statement);
extern (C) int raptor_serializer_set_namespace(raptor_serializer *rdf_serializer, raptor_uri *uri, char *prefix);
extern (C) int raptor_serializer_serialize_end(raptor_serializer *rdf_serializer);

/* Term */
extern (C)raptor_term * raptor_new_term_from_uri(raptor_world * world, raptor_uri * uri);
extern (C)raptor_term * raptor_new_term_from_counted_uri_string(raptor_world * world, char *uri_string, size_t length);
extern (C)raptor_term * raptor_new_term_from_uri_string(raptor_world * world, char *uri_string);
extern (C)raptor_term * raptor_new_term_from_literal(raptor_world * world, char *literal, raptor_uri * datatype, char *language);
extern (C)raptor_term * raptor_new_term_from_blank(raptor_world * world, char *blank);
extern (C)raptor_term * raptor_new_term_from_counted_blank(raptor_world * world, char *blank, size_t length);
extern (C)raptor_term * raptor_new_term_from_counted_string(raptor_world * world, char *string, size_t length);
extern (C)raptor_term * raptor_term_copy(raptor_term * term);
extern (C) int raptor_term_compare(raptor_term *t1, raptor_term *t2);
extern (C) int raptor_term_equals(raptor_term *t1, raptor_term *t2);
extern (C) void raptor_free_term(raptor_term *term);

/* Statement */
extern (C)raptor_statement *
raptor_new_statement_from_nodes(raptor_world * world, raptor_term * subject, raptor_term * predicate, raptor_term * object,
                                raptor_term * graph);

