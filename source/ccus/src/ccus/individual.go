package main

type TAG uint8

const (
	TAG_NONE TAG = 255

	TAG_TEXT_RU TAG = 42

	TAG_TEXT_EN TAG = 43
	/** date/time values in the standard format (UTF8 string, RFC3339). */
	TAG_STANDARD_DATE_TIME TAG = 0

	/** date/time values as Epoch timestamp (numeric, RFC3339). */
	TAG_EPOCH_DATE_TIME TAG = 1

	/** positive big integer value (byte string). */
	TAG_POSITIVE_BIGINT TAG = 2

	/** negative big integer value (byte string). */
	TAG_NEGATIVE_BIGINT TAG = 3

	/** decimal fraction value (two-element array, base 10). */
	TAG_DECIMAL_FRACTION TAG = 4

	/** big decimal value (two-element array, base 2). */
	TAG_BIGDECIMAL TAG = 5

	/** base64url encoding. */
	TAG_EXPECTED_BASE64_URL_ENCODED TAG = 21

	/** base64 encoding. */
	TAG_EXPECTED_BASE64_ENCODED TAG = 22

	/** base16 encoding. */
	TAG_EXPECTED_BASE16_ENCODED TAG = 23

	/** encoded CBOR data item (byte string). */
	TAG_CBOR_ENCODED TAG = 24

	/** URL (UTF8 string). */
	TAG_URI TAG = 32

	/** base64url encoded string (UTF8 string). */
	TAG_BASE64_URL_ENCODED TAG = 33

	/** base64 encoded string (UTF8 string). */
	BASE64_ENCODED TAG = 34

	/** regular expression string (UTF8 string, PCRE). */
	REGEXP TAG = 35

	/** MIME message (UTF8 string, RFC2045). */
	MIME_MESSAGE TAG = 36
)

type LANG uint8

const (
	/// Не установлен
	LANG_NONE LANG = 0

	/// Русский
	LANG_RU LANG = 1

	/// Английский
	LANG_EN LANG = 2
)

type DataType uint8

const (
	Uri DataType = 1

	/// Строка
	String DataType = 2

	/// Целочисленное число
	Integer DataType = 4

	/// Время
	Datetime DataType = 8

	/// Десятичное число
	Decimal DataType = 32

	/// Boolean
	Boolean DataType = 64
)

type Individual struct {
	uri       string
	resources map[string]Resources
}

func NewIndividual() *Individual {
	p := new(Individual)
	p.resources = make(map[string]Resources)
	return p
}

type Resource struct {
	_type DataType
	lang  LANG
	data  interface{}
}

type Resources []Resource

func NewResource() *Resource {
	p := new(Resource)
	p._type = Uri
	p.lang = LANG_NONE
	return p
}

func NewResource_type_value(in_type DataType, str string) *Resource {
	p := new(Resource)
	p._type = in_type
	p.lang = LANG_NONE
	p.data = str
	return p
}

func NewResource_value_lang(str string, lang LANG) *Resource {
	p := new(Resource)
	p._type = String
	p.lang = lang
	p.data = str
	return p
}

func NewResource_int64(num int64) *Resource {
	p := new(Resource)
	p._type = Integer
	p.data = num
	return p
}
func NewResource_uint64(num uint64) *Resource {
	p := new(Resource)
	p._type = Integer
	p.data = num
	return p
}
