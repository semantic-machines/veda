package main

import (
	"log"
	"reflect"
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

func ttResordToMap(uri string, tt_record map[interface{}]interface{}) Individual {
	individual := make(Individual)

	individual["@"] = uri
	for predicate, pp := range tt_record {

		switch reflect.TypeOf(pp).Kind() {
		case reflect.Slice:
			s := reflect.ValueOf(pp)

			resources := make([]interface{}, 0, s.Len())

			for i := 0; i < s.Len(); i++ {
				arr_val := s.Index(i)

				val1 := arr_val.Interface()
				val := reflect.ValueOf(val1)

				v_size := val.Len()

				if v_size == 0 || v_size > 3 {
					log.Printf("!ERR ttResordToMap: vsize < 1 || > 3 %d", v_size)
					continue
				}

				resource := make(map[string]interface{})

				vtype := DataType(val.Index(0).Interface().(uint64))

				switch vtype {
				case Uri:
					resource["type"] = dataTypeToString(Uri)
					resource["data"] = val.Index(1).Interface().(string)
				case Integer:
					resource["type"] = dataTypeToString(Integer)

					num := val.Index(1).Interface()
					switch num.(type) {
					case uint64:
						resource["data"] = num.(uint64)
					case int64:
						resource["data"] = num.(int64)
					default:
						log.Printf("!ERR unknown num type %v", num)
						return nil
					}

				case Datetime:
					r_dt := val.Index(1).Interface()

					switch r_dt.(type) {
					case int64:
						resource["data"] = time.Unix(r_dt.(int64), 0).UTC().Format("2006-01-02T15:04:05Z")
					case uint64:
						resource["data"] = time.Unix(int64(r_dt.(uint64)), 0).UTC().Format("2006-01-02T15:04:05Z")
					default:
						log.Printf("!ERR ttResordToMap: NOT INT/UINT IN DATETIME: %v", r_dt)
						return nil
					}
					resource["type"] = dataTypeToString(Datetime)

				case Decimal:
					var mantissa, exponent int64

					r_mantissa := val.Index(1).Interface()
					switch r_mantissa.(type) {
					case uint64:
						mantissa = int64(r_mantissa.(uint64))
					case int64:
						mantissa = r_mantissa.(int64)
					default:
						log.Printf("!ERR ttResordToMap: unknown num type %v", r_mantissa)
						return nil
					}

					r_exponent := val.Index(2).Interface()
					switch r_exponent.(type) {
					case uint64:
						exponent = int64(r_exponent.(uint64))
					case int64:
						exponent = r_exponent.(int64)
					default:
						log.Printf("!ERR ttResordToMap: unknown num type %v", r_exponent)
						return nil
					}

					resource["type"] = dataTypeToString(Decimal)
					resource["data"] = decimalToString(mantissa, exponent)

				case Boolean:
					resource["type"] = dataTypeToString(Boolean)
					resource["data"] = val.Index(1).Interface().(bool)

				case String:
					rval := val.Index(1).Interface()

					switch rval.(type) {
					case string:
						resource["type"] = dataTypeToString(String)
						resource["data"] = rval.(string)

						if v_size == 3 {

							r_lang := val.Index(2).Interface()
							switch r_lang.(type) {
							case uint64:
								resource["lang"] = langToString(Lang(r_lang.(uint64)))
							case int64:
								resource["lang"] = langToString(Lang(r_lang.(int64)))
							default:
								log.Printf("!ERR ttResordToMap: unknown lang type %v", r_lang)
								return nil
							}
						}
					case nil:
						resource["type"] = dataTypeToString(String)
						resource["data"] = ""
					default:
						log.Printf("!ERR ttResordToMap: unknown string type %v", rval)
						return nil
					}

				default:
					log.Printf("!ERR ttResordToMap: unknown type %v", vtype)
					return nil
				}
				resources = append(resources, resource)

			}
			individual[predicate.(string)] = resources
		}
		//resources = append(resources, resource)
		//individual[key.(string)] = v
	}

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
