package main

import (
	"log"
	"reflect"
	"sort"
	"strconv"
	"time"
)

type Individual map[string]interface{}

//DataType represents Resource type in veda
type DataType uint8

const (
	//Uri is string with uri of another individual
	Uri DataType = 1
	//String is simple text
	String DataType = 2
	//Integer is simple number
	Integer DataType = 4
	//Datetime represents datetime in unix timestamp
	Datetime DataType = 8
	//Decimal consists of exponent and mantissa in decimal form
	Decimal DataType = 32
	//Boolean is simple boolean data
	Boolean DataType = 64
	//Unknown used for errors
	Unknown DataType = 0
)

//Lang is string resource language code
type Lang uint8

const (
	//LangNone used if no language set
	LangNone Lang = 0
	//LangRu set for russian language text
	LangRu Lang = 1
	//LangEn set for english language text
	LangEn Lang = 2
)

const (
	_HASH      = 0
	_SUBJECT   = 1
	_PREDICATE = 2
	_OBJECT    = 3
	_TYPE      = 4
	_LANG      = 5
	_ORDER     = 6
)

type Resources []interface{}

func (a Resources) Len() int      { return len(a) }
func (a Resources) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a Resources) Less(i, j int) bool {
	vi := a[i].(map[string]interface{})
	vj := a[j].(map[string]interface{})

	ni := vi["order"]
	nj := vj["order"]

	if ni == nil {
		ni = 0
	}

	if nj == nil {
		nj = 0
	}
	return ni.(int64) < nj.(int64)
}

func getFirstInt(indv Individual, predicate string) (int, bool) {
	rss, err := indv[predicate].([]interface{})
	if err != true {
		return 0, false
	}

	_data := rss[0].(map[string]interface{})["data"]

	switch _data.(type) {
	case int64:
		return int(_data.(int64)), true
	case uint64:
		return int(_data.(uint64)), true
	default:
		return 0, false
	}
}

func getFirstString(indv Individual, predicate string) (string, bool) {
	rss, err := indv[predicate].([]interface{})
	if err != true {
		return "", false
	}

	_data := rss[0].(map[string]interface{})["data"]

	switch _data.(type) {
	case string:
		return _data.(string), true
	default:
		return "", false
	}
}

func getFirstBool(indv Individual, predicate string) (bool, bool) {
	rss, err := indv[predicate].([]interface{})
	if err != true {
		return false, false
	}

	_data := rss[0].(map[string]interface{})["data"]

	switch _data.(type) {
	case bool:
		return _data.(bool), true
	default:
		return false, false
	}
}

func getUri(indv Individual) string {
	return indv["@"].(string)
}

func ttResordToMap(uri string, tt_record []interface{}) Individual {
	individual := make(Individual)

	//log.Printf("@ttresord tt_record=%v\n", tt_record)

	individual["@"] = uri
	for _, pp := range tt_record {
		// loop of records

		var subject string
		var predicate string
		var str_object string
		var num_object int64
		var bool_object bool
		var _type DataType
		var _lang Lang
		var order int64 = -1

		switch reflect.TypeOf(pp).Kind() {
		case reflect.Slice:
			s := reflect.ValueOf(pp)

			for irow := 0; irow < s.Len(); irow++ {
				// loop of fields

				arr_val := s.Index(irow)

				val1 := arr_val.Interface()
				val := reflect.ValueOf(val1)

				if val.IsValid() == true {
				vval := val.Interface()

				var sval string
				var nval int64

				//log.Printf("@vval =%v\n", vval)

				switch vval.(type) {
				case bool:
					bb := vval.(bool)
					if irow == _OBJECT {
						bool_object = bb
					}
				case uint64:
					nval = int64(vval.(uint64))
					if irow == _OBJECT {
						num_object = nval
					}
				case int64:
					nval = vval.(int64)
					if irow == _OBJECT {
						num_object = nval
					}
				case string:
					sval = vval.(string)
					if irow == _OBJECT {
						str_object = sval
					}
				default:
					log.Printf("!ERR unknown num type %v\n", vval)
					return nil
				}

				//log.Printf("@sval =%v, nval=%v\n", sval, nval)

				if irow == _ORDER {
					order = nval
				}

				if irow == _SUBJECT && subject == "" {
					subject = sval
				}

				if irow == _PREDICATE && predicate == "" {
					predicate = sval
				}

				if irow == _TYPE && _type == 0 {
					_type = DataType(nval)
				}

				if irow == _LANG && _lang == 0 {
					_lang = Lang(nval)
				}
			    }
			}

			var resources []interface{}
			presources := individual[predicate]
			if presources == nil {
				resources = make([]interface{}, 0, 1)
				//individual[predicate] = resources
			} else {
				resources = presources.([]interface{})
			}

			//log.Printf("@ttresord predicate=%v\n", predicate)
			//log.Printf("@ttresord type=%v\n", _type)
			resource := make(map[string]interface{})
			resource["type"] = dataTypeToString(_type)
			resource["order"] = order

			if _type == Uri {
				resource["data"] = str_object
			} else if _type == String {
				resource["data"] = str_object
				resource["lang"] = langToString(_lang)
			} else if _type == Integer {
				resource["data"] = num_object
			} else if _type == Datetime {
				resource["data"] = time.Unix(int64(num_object), 0).UTC().Format("2006-01-02T15:04:05Z")
			} else if _type == Decimal {
				resource["data"] = str_object
			} else if _type == Boolean {
				resource["data"] = bool_object
			}

			resources = append(resources, resource)
			individual[predicate] = resources
		}
	}

	for predicate := range individual {

		if predicate != "@" {
			// reorder predicate values
			var resources []interface{}
			presources := individual[predicate]
			if presources != nil {
				resources = presources.([]interface{})
			}

			pcount := len(resources)

			if pcount == 2 {
				a := resources[0].(map[string]interface{})
				b := resources[1].(map[string]interface{})

				an := a["order"]
				bn := b["order"]

				if an != nil && bn != nil && an.(int64) > bn.(int64) {
					resources[0] = b
					resources[1] = a
				}
			} else if pcount > 2 {
				sort.Sort(Resources(resources))
			}

			for irow := 0; irow < pcount; irow++ {
				a := resources[0].(map[string]interface{})
				delete(a, "order")
			}
		}
	}

	//log.Printf("@ttresord %v\n", individual)

	return individual
}

//dataTypeToString converts DataType variable to its string representations
func dataTypeToString(dataType DataType) string {
	switch dataType {
	case Uri:
		return "Uri"
	case String:
		return "String"
	case Integer:
		return "Integer"
	case Datetime:
		return "Datetime"
	case Decimal:
		return "Decimal"
	case Boolean:
		return "Boolean"
	}

	log.Println("ERR! UNKNOWN DATA TYPE")
	return ""
}

//langToString converts Lang variable to its string representation
func langToString(lang Lang) string {
	switch lang {
	case LangRu:
		return "RU"
	case LangEn:
		return "EN"
	default:
		return "NONE"
	}
}

//decimalToString converts Decimal to string representation of number with floating point
func decimalToString(mantissa, exponent int64) string {
	negative := false
	res := make([]rune, 0)
	if mantissa < 0 {
		negative = true
		mantissa = -mantissa
	}

	res = []rune(strconv.FormatInt(mantissa, 10))
	if exponent >= 0 {
		zeros := make([]rune, exponent)
		for i := 0; i < int(exponent); i++ {
			zeros[i] = '0'
		}

		res = append(res, zeros...)
	} else {
		exponent = -exponent
		if len(res) > int(exponent) {
			tmp := make([]rune, 0, len(res)+1)
			tmp = append(tmp, res[:len(res)-int(exponent)]...)
			tmp = append(tmp, '.')
			tmp = append(tmp, res[len(res)-int(exponent):]...)
			res = tmp
		} else {
			zeros := make([]rune, exponent)
			zeros[0] = '.'
			for i := 1; i < int(exponent); i++ {
				zeros[i] = '0'
			}
			res = append(zeros, res...)
		}
	}

	if negative {
		return "-" + string(res)
	}

	return string(res)
}
