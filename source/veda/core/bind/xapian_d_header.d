/**
   обвязка к xapin

   Copyright: © 2014 Semantic Machines
   License: Subject to the terms of the MIT license, as written in the included LICENSE.txt file.
   Authors: Valeriy Bushenev

 */

module bind.xapian_d_header;


/// Enum of stem strategy
enum stem_strategy { STEM_NONE, STEM_SOME, STEM_ALL, STEM_ALL_Z };

/// Enum of possible query operations
enum xapian_op
{
    /// Return iff both subqueries are satisfied
    OP_AND,

    /// Return if either subquery is satisfied
    OP_OR,

    /// Return if left but not right satisfied
    OP_AND_NOT,

    /// Return if one query satisfied, but not both
    OP_XOR,

    /// Return iff left satisfied, but use weights from both
    OP_AND_MAYBE,

    /// As AND, but use only weights from left subquery
    OP_FILTER,

    /** Find occurrences of a list of terms with all the terms
     *  occurring within a specified window of positions.
     *
     *  Each occurrence of a term must be at a different position,
     *  but the order they appear in is irrelevant.
     *
     *  The window parameter should be specified for this operation,
     *  but will default to the number of terms in the list.
     */
    OP_NEAR,

    /** Find occurrences of a list of terms with all the terms
     *  occurring within a specified window of positions, and all
     *  the terms appearing in the order specified.
     *
     *  Each occurrence of a term must be at a different position.
     *
     *  The window parameter should be specified for this operation,
     *  but will default to the number of terms in the list.
     */
    OP_PHRASE,

    /** Filter by a range test on a document value. */
    OP_VALUE_RANGE,

    /** Scale the weight of a subquery by the specified factor.
     *
     *  A factor of 0 means this subquery will contribute no weight to
     *  the query - it will act as a purely boolean subquery.
     *
     *  If the factor is negative, Xapian::InvalidArgumentError will
     *  be thrown.
     */
    OP_SCALE_WEIGHT,

    /** Pick the best N subqueries and combine with OP_OR.
     *
     *  If you want to implement a feature which finds documents
     *  similar to a piece of text, an obvious approach is to build an
     *  "OR" query from all the terms in the text, and run this query
     *  against a database containing the documents.  However such a
     *  query can contain a lots of terms and be quite slow to perform,
     *  yet many of these terms don't contribute usefully to the
     *  results.
     *
     *  The OP_ELITE_SET operator can be used instead of OP_OR in this
     *  situation.  OP_ELITE_SET selects the most important ''N'' terms
     *  and then acts as an OP_OR query with just these, ignoring any
     *  other terms.  This will usually return results just as good as
     *  the full OP_OR query, but much faster.
     *
     *  In general, the OP_ELITE_SET operator can be used when you have
     *  a large OR query, but it doesn't matter if the search
     *  completely ignores some of the less important terms in the
     *  query.
     *
     *  The subqueries don't have to be terms, but if they aren't then
     *  OP_ELITE_SET will look at the estimated frequencies of the
     *  subqueries and so could pick a subset which don't actually
     *  match any documents even if the full OR would match some.
     *
     *  You can specify a parameter to the query constructor which
     *  control the number of terms which OP_ELITE_SET will pick.  If
     *  not specified, this defaults to 10 (or
     *  <code>ceil(sqrt(number_of_subqueries))</code> if there are more
     *  than 100 subqueries, but this rather arbitrary special case
     *  will be dropped in 1.3.0).  For example, this will pick the
     *  best 7 terms:
     *
     *  <pre>
     *  Xapian::Query query(Xapian::Query::OP_ELITE_SET, subqs.begin(), subqs.end(), 7);
     *  </pre>
     *
     * If the number of subqueries is less than this threshold,
     * OP_ELITE_SET behaves identically to OP_OR.
     */
    OP_ELITE_SET,

    /** Filter by a greater-than-or-equal test on a document value. */
    OP_VALUE_GE,

    /** Filter by a less-than-or-equal test on a document value. */
    OP_VALUE_LE,

    /** Treat a set of queries as synonyms.
     *
     *  This returns all results which match at least one of the
     *  queries, but weighting as if all the sub-queries are instances
     *  of the same term: so multiple matching terms for a document
     *  increase the wdf value used, and the term frequency is based on
     *  the number of documents which would match an OR of all the
     *  subqueries.
     *
     *  The term frequency used will usually be an approximation,
     *  because calculating the precise combined term frequency would
     *  be overly expensive.
     *
     *  Identical to OP_OR, except for the weightings returned.
     */
    OP_SYNONYM
};

/// Enum of feature flag
enum feature_flag
{
    /// Support AND, OR, etc and bracketed subexpressions.
    FLAG_BOOLEAN                 = 1,
    /// Support quoted phrases.
    FLAG_PHRASE                  = 2,
    /// Support + and -.
    FLAG_LOVEHATE                = 4,
    /// Support AND, OR, etc even if they aren't in ALLCAPS.
    FLAG_BOOLEAN_ANY_CASE        = 8,
    /** Support right truncation (e.g. Xap*).
     *
     *  Currently you can't use wildcards with boolean filter prefixes,
     *  or in a phrase (either an explicitly quoted one, or one implicitly
     *  generated by hyphens or other punctuation).
     *
     *  NB: You need to tell the QueryParser object which database to
     *  expand wildcards from by calling set_database.
     */
    FLAG_WILDCARD                = 16,
    /** Allow queries such as 'NOT apples'.
     *
     *  These require the use of a list of all documents in the database
     *  which is potentially expensive, so this feature isn't enabled by
     *  default.
     */
    FLAG_PURE_NOT                = 32,
    /** Enable partial matching.
     *
     *  Partial matching causes the parser to treat the query as a
     *  "partially entered" search.  This will automatically treat the
     *  final word as a wildcarded match, unless it is followed by
     *  whitespace, to produce more stable results from interactive
     *  searches.
     *
     *  Currently FLAG_PARTIAL doesn't do anything if the final word
     *  in the query has a boolean filter prefix, or if it is in a phrase
     *  (either an explicitly quoted one, or one implicitly generated by
     *  hyphens or other punctuation).  It also doesn't do anything if
     *  if the final word is part of a value range.
     *
     *  NB: You need to tell the QueryParser object which database to
     *  expand wildcards from by calling set_database.
     */
    FLAG_PARTIAL                 = 64,

    /** Enable spelling correction.
     *
     *  For each word in the query which doesn't exist as a term in the
     *  database, Database::get_spelling_suggestion() will be called and if
     *  a suggestion is returned, a corrected version of the query string
     *  will be built up which can be read using
     *  QueryParser::get_corrected_query_string().  The query returned is
     *  based on the uncorrected query string however - if you want a
     *  parsed query based on the corrected query string, you must call
     *  QueryParser::parse_query() again.
     *
     *  NB: You must also call set_database() for this to work.
     */
    FLAG_SPELLING_CORRECTION     = 128,

    /** Enable synonym operator '~'.
     *
     *  NB: You must also call set_database() for this to work.
     */
    FLAG_SYNONYM                 = 256,

    /** Enable automatic use of synonyms for single terms.
     *
     *  NB: You must also call set_database() for this to work.
     */
    FLAG_AUTO_SYNONYMS           = 512,

    /** Enable automatic use of synonyms for single terms and groups of
     *  terms.
     *
     *  NB: You must also call set_database() for this to work.
     */
    FLAG_AUTO_MULTIWORD_SYNONYMS = 1024 | FLAG_AUTO_SYNONYMS,

    /** The default flags.
     *
     *  Used if you don't explicitly pass any to @a parse_query().
     *  The default flags are FLAG_PHRASE|FLAG_BOOLEAN|FLAG_LOVEHATE.
     *
     *  Added in Xapian 1.0.11.
     */
    FLAG_DEFAULT                 = FLAG_PHRASE | FLAG_BOOLEAN | FLAG_LOVEHATE
};

alias int   size_t;
alias int   int32_t;
alias byte  int8_t;
alias ubyte uint8_t;
alias dchar TCHAR;

/** Open for read/write; create if no db exists. */
const int DB_CREATE_OR_OPEN      = 1;
/** Create a new database; fail if db exists. */
const int DB_CREATE              = 2;
/** Overwrite existing db; create if none exists. */
const int DB_CREATE_OR_OVERWRITE = 3;
/** Open for read/write; fail if no db exists. */
const int DB_OPEN                = 4;

const int BRASS                  = 1;
const int CHERT                  = 2;
const int IN_MEMORY              = 3;

extern (C++)
{
/// Base class for value range processors
interface XapianNumberValueRangeProcessor
{
}

interface XapianTermIterator
{
    void reset(byte *err);
    void next(byte *err);
    bool is_next(byte *err);
    void get_term(char **out_val, uint **out_val_length, byte *err);
}

/// Parses a piece of text and generate terms.
interface XapianTermGenerator
{
    /// Set the XapianStem object to be used for generating stemmed terms.
    void set_stemmer(XapianStem stemmer, byte *err);

    /// -
    void set_document(XapianDocument doc, byte *err);

    /// -
    void index_text(const char *data_str, ulong data_len, byte *err);

    /// -
    void index_text(const char *data_str, ulong data_len, const char *prefix_str, ulong prefix_len, byte *err);

    /// -
    void index_text_without_positions(const char *data_str, ulong data_len, byte *err);

    /// -
    void index_text_without_positions(const char *data_str, ulong data_len, const char *prefix_str, ulong prefix_len, byte *err);

    /// -
    void index_data(int data, const char *prefix_str, ulong prefix_len, byte *err);

    /// -
    void index_data(long data, const char *prefix_str, ulong prefix_len, byte *err);

    /// -
    void index_data(float data, const char *prefix_str, ulong prefix_len, byte *err);

    /// -
    void index_data(double data, const char *prefix_str, ulong prefix_len, byte *err);
}

/// This class is used to access a database, or a group of databases.
interface XapianDatabase
{
    XapianEnquire new_Enquire(byte *err);
    void close(byte *err);
    void reopen(byte *err);
    XapianTermIterator allterms(const char *prefix_str, ulong prefix_len, byte *err);
    void add_database(XapianDatabase add_db, byte *err);
}

/// This class provides read/write access to a database
interface XapianWritableDatabase
{
    XapianEnquire new_Enquire(byte *err);
    uint add_document(XapianDocument doc, byte *err);
    uint replace_document(const char *_unique_term, ulong _unique_term_len, XapianDocument document, byte *err);
    void delete_document(const char *_unique_term, ulong _unique_term_len, byte *err);
    void commit(byte *err);
    void close(byte *err);
    void reopen(byte *err);
}

/// Class representing a query
interface XapianQuery
{
    void get_description(char **out_val, uint **out_val_length, byte *err);
    void serialise(char **out_val, uint **out_val_length, byte *err);
    XapianQuery add_right_query(int op_, XapianQuery _right, byte *err);
}

/// A handle representing a document in a Xapian database
interface XapianDocument
{
    char *get_data(char **out_val, uint **out_val_length, byte *err);
    void set_data(const char *data_str, ulong data_len, byte *err);
    void add_boolean_term(const char *_data, ulong _data_len, byte *err);
    void add_value(int slot, const char *_data, ulong _data_len, byte *err);
    void add_value(int slot, int _data, byte *err);
    void add_value(int slot, long _data, byte *err);
    void add_value(int slot, float _data, byte *err);
    void add_value(int slot, double _data, byte *err);
}

/// An iterator pointing to items in an MSet
interface XapianMSetIterator
{
    uint get_documentid(byte *err);
    XapianDocument get_document(byte *err);
    void get_document_data(char **out_val, uint **out_val_length, byte *err);

    void next(byte *err);
    bool is_next(byte *err);
}

/// A match set (MSet)
interface XapianMSet
{
    int get_matches_estimated(byte *err);
    int size(byte *err);
    XapianMSetIterator iterator(byte *err);
}

/// This class provides an interface to the information retrieval system for the purpose of searching
interface XapianEnquire
{
    void set_query(XapianQuery query, byte *err);
    XapianMSet get_mset(int from, int size, byte *err);
    void set_sort_by_key(XapianMultiValueKeyMaker sorter, bool p, byte *err);
    void clear_matchspies();
}

/// Class representing a stemming algorithm
interface XapianStem
{
}

/// Build a XapianQuery object from a user query string
interface XapianQueryParser
{
    void set_stemmer(XapianStem stemmer, byte *err);
    void set_database(XapianDatabase db, byte *err);
    void set_database(XapianWritableDatabase db, byte *err);
    void set_stemming_strategy(stem_strategy strategy, byte *err);
    XapianQuery parse_query(char *query_string, ulong query_string_len, byte *err);
    XapianQuery parse_query(char *query_string, ulong query_string_len, uint flags, byte *err);
    XapianQuery parse_query(char *query_string, ulong query_string_len, uint flags, char *prefix_string, ulong prefix_string_len,
                            byte *err);
    void add_prefix(char *field_string, ulong field_string_len, char *prefix_string, ulong prefix_string_len, byte *err);
    void add_valuerangeprocessor(XapianNumberValueRangeProcessor pp, byte *err);
}

/// KeyMaker subclass which combines several values
interface XapianMultiValueKeyMaker
{
    void add_value(int pos, byte *err);
    void add_value(int pos, bool asc_desc, byte *err);
}

/// -
XapianDatabase new_Database(byte *err);

/// -
XapianDatabase new_Database(const char *path, uint path_len, int db_type, byte *err);

/// -
XapianWritableDatabase new_WritableDatabase(const char *path, uint path_len, int action, int db_type, byte *err);

/// -
XapianDocument new_Document(byte *err);

/// -
XapianMultiValueKeyMaker new_MultiValueKeyMaker(byte *err);

/// -
XapianQueryParser new_QueryParser(byte *err);

/// -
XapianStem new_Stem(char *language, uint language_len, byte *err);

/// -
XapianTermGenerator new_TermGenerator(byte *err);

/// -
XapianNumberValueRangeProcessor new_NumberValueRangeProcessor(int slot, const char *_str, ulong _str_len, bool prefix, byte *err);

/// -
XapianQuery new_Query(byte *err);

/// -
XapianQuery new_Query(const char *_str, uint _str_len, byte *err);
//    XapianQuery new_Query_add (XapianQuery _left, XapianQuery _right);//, int op_);
/// -
XapianQuery new_Query_range(int op_, int slot, double _begin, double _end, byte *err);

/// -
XapianQuery new_Query_double(int op_, int slot, double _value, byte *err);

/// -
XapianQuery new_Query_equal(int op_, int slot, const char *_str, ulong _str_len, byte *err);

/// -
void sortable_serialise(double value, char **out_val, uint **out_val_length, byte *err);

////////

/// -
void destroy_Document(XapianDocument doc);

/// -
void destroy_MSet(XapianMSet mset);

/// -
void destroy_MSetIterator(XapianMSetIterator msetit);

/// -
void destroy_Query(XapianQuery query);

/// -
void destroy_Enquire(XapianEnquire enquire);

/// -
void destroy_MultiValueKeyMaker(XapianMultiValueKeyMaker sorter);

/// -
void destroy_Database(XapianDatabase db);
}

class XapianError : Exception
{
    byte code = 0;

    this(byte _code, string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        super(msg, file, line, next);
        code = _code;
    }

    string get_xapian_msg()
    {
        return xapian_msg_code.get(code, "Unknown");
    }
}

private string[ byte ] xapian_msg_code;

static this()
{
    xapian_msg_code =
    [
        -1:"DatabaseModifiedError",
        -2:"DatabaseLockError"
        ,
        -3:"LogicError"
        ,
        -4:"AssertionError"
        ,
        -5:"InvalidArgumentError"
        ,
        -6:"InvalidOperationError"
        ,
        -7:"UnimplementedError"
        ,
        -8:"RuntimeError"
        ,
        -9:"DatabaseError"
        ,
        -10:"DatabaseCorruptError"
        ,
        -11:"DatabaseCreateError"
        ,
        -12:"DatabaseOpeningError"
        ,
        -13:"DatabaseVersionError"
        ,
        -14:"DocNotFoundError"
        ,
        -15:"FeatureUnavailableError"
        ,
        -16:"InternalError"
        ,
        -17:"NetworkError"
        ,
        -18:"NetworkTimeoutError"
        ,
        -19:"QueryParserError"
        ,
        -20:"RangeError"
        ,
        -21:"SerialisationError"
    ];
}

