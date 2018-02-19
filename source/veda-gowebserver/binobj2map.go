package main

import (
	"bufio"
	"cbor"
	"fmt"
	"log"
	"reflect"
	"strings"
	"time"
	"strconv"
	"bytes"
	"gopkg.in/vmihailenco/msgpack.v2"
)

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

	log.Println("@ERR UNKNOWN DATA TYPE")
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

//String to decimal converts string representation of number with floating point to Veda Decimal
func stringToDecimal(str string) (int64, int64) {
	var exponent, mantissa int64

	runes := []rune(str)

	i := 0
	negative := false
	if runes[0] == '-' {
		i++
		negative = true
	}

	j := len(runes) - 1
	for ; j >= i; j-- {
		if runes[j] != '0' {
			break
		}

		exponent++
	}

	expStep := int64(0)
	for ; i <= j; i++ {
		if runes[i] == '.' {
			expStep = -1
			continue
		}

		number, _ := strconv.ParseInt(string(runes[i]), 10, 64)
		mantissa = mantissa*10 + number
		exponent += expStep
	}

	if negative {
		mantissa = -mantissa
	}

	return mantissa, exponent
}

//stringToDataType converts string representation to DataType value
func stringToDataType(str string) DataType {
	switch str {
	case "Uri":
		return Uri
	case "String":
		return String
	case "Integer":
		return Integer
	case "Datetime":
		return Datetime
	case "Decimal":
		return Decimal
	case "Boolean":
		return Boolean
	}

	log.Println("@ERR UNKNOWN DATA TYPE STRING")
	return Unknown
}

//stringToLang converts string representation to Lang value
func stringToLang(str string) Lang {
	switch str {
	case "RU":
		return LangRu
	case "EN":
		return LangEn
	default:
		return LangNone
	}
}

//MsgpackToMap converts msgpack from tarantool to json map representation of veda individual
func BinobjToMap(binobjStr string) map[string]interface{} {
	if binobjStr[0] == 0xFF {
			return MsgpackToMap(binobjStr)
		} else {
			return CborToMap(binobjStr)			
		}	
}

//MapToMsgpack converts individual map from client to msgpack for sending to tarantool
func MapToMsgpack(jsonMap map[string]interface{}) string {
	var buf bytes.Buffer

	//Encodes array len for uri and resources map
	writer := bufio.NewWriter(&buf)
	encoder := msgpack.NewEncoder(writer)
	encoder.EncodeArrayLen(2)
	//Encodes uri, in client @ is used as uri key
	encoder.Encode(jsonMap["@"])
	encoder.EncodeMapLen(len(jsonMap) - 1)

	for k, v := range jsonMap {
		if k == "@" {
			continue
		}

		resources := v.([]interface{})
		//Encode resource key and array len for resource values
		encoder.Encode(k)
		encoder.EncodeArrayLen(len(resources))
		for i := 0; i < len(resources); i++ {
			//Get resource type and check it
			resource := resources[i].(map[string]interface{})
			datatype := Unknown
			//Sometimes its float64 sometimes string
			switch resource["type"].(type) {
			case float64:
				datatype = DataType(resource["type"].(float64))
			case string:
				datatype = stringToDataType(resource["type"].(string))
			}

			switch datatype {
			//Integer, Uri, Boolean are simply encoded into array
			case Uri:
				encoder.Encode(resource["data"].(string))
			case Integer:
				encoder.Encode(int64(resource["data"].(float64)))
			case Datetime:
				datetime, _ := time.Parse("2006-01-02T15:04:05.000Z", resource["data"].(string))
				encoder.EncodeArrayLen(2)
				encoder.Encode(Datetime, datetime)
			//Decimal need array with len 3 for type, mantissa and exponent
			case Decimal:
				mantissa, exponent := stringToDecimal(resource["data"].(string))
				encoder.EncodeArrayLen(3)
				encoder.Encode(Decimal, mantissa, exponent)
			case Boolean:
				encoder.Encode(resource["data"].(bool))
			//Strings need encode one more array to encode language
			case String:
				lang := LangNone
				switch resource["lang"].(type) {
				case float64:
					lang = Lang(resource["lang"].(float64))
				case string:
					lang = stringToLang(resource["lang"].(string))
				}
				//If lang set to LangRu or LangEn then encode array with len 3 and encode,
				//type, data and lang. Else use array with len 2 to encode type and data.
				if lang != LangNone {
					encoder.EncodeArrayLen(3)
					encoder.Encode(String, resource["data"].(string), lang)
				} else {
					encoder.EncodeArrayLen(2)
					encoder.Encode(String, resource["data"].(string))
				}
			}
		}
	}

	writer.Flush()
	return string(buf.Bytes())
}

func prepareElement(v interface{}) (interface{}, error) {
	switch v.(type) {

	case uint64, int64:
		return map[string]interface{}{
			"data": v,
			"type": dataTypeToString(Integer),
		}, nil

		// case float32:
		// log.Printf("float32 %f", float64(i))
		// return true, ""
		// case float64:
		// log.Printf("float64 %f", i)
		// return true, ""
	case bool:
		return map[string]interface{}{
			"data": v,
			"type": dataTypeToString(Boolean),
		}, nil
	case string:
		return map[string]interface{}{
			"data": v,
			"type": dataTypeToString(String),
		}, nil

	case cbor.CBORTag:
		wrappedVal := v.(cbor.CBORTag).WrappedObject
		tag := TAG(v.(cbor.CBORTag).Tag)
		//		log.Printf("tag #1 subject_uri=%s, predicate_uri=%s", subject_uri, predicate_uri)

		if tag == TAG_TEXT_RU {
			return map[string]interface{}{
				"data": wrappedVal,
				"type": dataTypeToString(String),
				"lang": langToString(LangRu),
			}, nil
		} else if tag == TAG_TEXT_EN {
			return map[string]interface{}{
				"data": wrappedVal,
				"type": dataTypeToString(String),
				"lang": langToString(LangEn),
			}, nil
		} else if tag == TAG_URI {
			return map[string]interface{}{
				"data": wrappedVal,
				"type": dataTypeToString(Uri),
			}, nil
		} else if tag == TAG_DECIMAL_FRACTION {
			log.Fatalln("decimal ", wrappedVal)
			arr := wrappedVal.([]interface{})
			mantissa := arr[0].(int64)
			exponent := arr[1].(int64)
			return map[string]interface{}{
				"data": decimalToString(mantissa, exponent),
				"type": dataTypeToString(Decimal),
			}, nil
		} else if tag == TAG_EPOCH_DATE_TIME {
			return map[string]interface{}{
				"data": time.Unix(int64(wrappedVal.(uint64)), 0).Format("2006-01-02T15:04:05Z"),
				"type": dataTypeToString(Decimal),
			}, nil
		}

		log.Fatalf("TAG: %v\n", tag)
		return nil, fmt.Errorf("Unsupported tag: %v", tag)

	default:
		return nil, fmt.Errorf("Unsupported type: %v", reflect.TypeOf(v))
	}
}

func CborToMap(cborStr string) map[string]interface{} {
	individual := make(map[string]interface{})

	ring := cbor.NewDecoder(strings.NewReader(cborStr))
	var cborObject interface{}
	err := ring.Decode(&cborObject)
	if err != nil {
		log.Println("@ERR DECODING CBOR OBJECT")
		return individual
	}

	var individualMap map[interface{}]interface{}
	switch cborObject.(type) {
	case map[interface{}]interface{}:
		individualMap = cborObject.(map[interface{}]interface{})
	default:
		log.Printf("@ERR CBOR: DECODING INIVIDUAL MAP: INTERFACE IS TYPE OF %v\n", reflect.TypeOf(cborObject))
		return individual
	}

	//log.Println(cborStr)
	for k, v := range individualMap {

		//log.Println(k)
		keyStr := k.(string)
		if keyStr == "@" {
			individual["@"] = v.(string)
			continue
		}

		switch v.(type) {
		case []interface{}:
			arr := v.([]interface{})
			resources := make([]interface{}, 0, len(arr))
			for i := 0; i < len(arr); i++ {

				resource, err := prepareElement(arr[i])
				if err != nil {
					log.Println(cborStr)
					log.Println(keyStr)
					log.Fatalln(err)
				}
				resources = append(resources, resource)
			}

			individual[keyStr] = resources
		case nil:
			continue

		default:
			resource, err := prepareElement(v)
			if err != nil {
				log.Fatalln(err)
			}

			individual[keyStr] = []interface{}{resource}
		}
	}

	return individual
}

//MsgpackToMap converts msgpack from tarantool to json map representation of veda individual
func MsgpackToMap(msgpackStr string) map[string]interface{} {
		//Allocate map and decode msgpack
		individual := make(map[string]interface{})
		decoder := msgpack.NewDecoder(strings.NewReader(msgpackStr[1:len (msgpackStr)]))
		decoder.DecodeArrayLen()

		// log.Printf("@MSGPACK %v\n", msgpackStr)

		//Set individual uri and decode map of resources
		individual["@"], _ = decoder.DecodeString()
		resMapI, err := decoder.DecodeMap()
		
		if err != nil {
			log.Fatalln(err)
			return nil
		}
		
		resMap := resMapI.(map[interface{}]interface{})
		// log.Println("@URI ", individual["@"])
		for keyI, resArrI := range resMap {
			// log.Printf("\t@PREDICATE %v\n", keyI)

			//Decode resource name
			predicate := keyI.(string)

			// log.Println("\t", predicate, resArrI)

			//Decode resource values and allocate resources arrat
			resArr := resArrI.([]interface{})
			resources := make([]interface{}, 0, len(resArr))

			for i := 0; i < len(resArr); i++ {
				//Decode resource and check its type
				resI := resArr[i]
				// log.Printf("\t\t@RES %v : %v\n", resI, reflect.TypeOf(resI))
				resource := make(map[string]interface{})
				switch resI.(type) {
				//Arrays can contain strings and date time
				case []interface{}:
					resArrI := resI.([]interface{})
					//Arrays of len tow can be date time or string without lang
					//Arrays of len three can be String with lang or Decimal
					if len(resArrI) == 2 {
						resType := DataType(resArrI[0].(uint64))
						if resType == Datetime {
							switch resArrI[1].(type) {
							case int64:
								resource["data"] = time.Unix(resArrI[1].(int64), 0).Format("2006-01-02T15:04:05Z")
							case uint64:
								resource["data"] = time.Unix(int64(resArrI[1].(uint64)), 0).Format("2006-01-02T15:04:05Z")
							default:
								log.Printf("@ERR SIZE 2! NOT INT/UINT IN DATETIME: %s\n",
									reflect.TypeOf(resArrI[1]))
								return nil
							}
							resource["type"] = dataTypeToString(Datetime)
						} else if resType == String {
							// fmt.Println("TRY TO DECODE STR")
							switch resArrI[1].(type) {
							case string:
								resource["data"] = resArrI[1]
							case nil:
								resource["data"] = ""
							default:
								log.Printf("@ERR SIZE 2! NOT STRING: %s\n",
									reflect.TypeOf(resArrI[1]))
								return individual
							}
							resource["type"] = dataTypeToString(String)
							resource["lang"] = langToString(LangNone)
						}
					} else if len(resArrI) == 3 {
						resType := DataType(resArrI[0].(uint64))

						if resType == Decimal {
							var mantissa, exponent int64

							switch resArrI[1].(type) {
							case int64:
								mantissa = resArrI[1].(int64)
							case uint64:
								mantissa = int64(resArrI[1].(uint64))
							default:
								log.Println("@ERR SIZE 3! NOT INT/UINT IN MANTISSA: %s\n",
									reflect.TypeOf(resArrI[1]))
								return nil
							}

							switch resArrI[2].(type) {
							case int64:
								exponent = resArrI[2].(int64)
							case uint64:
								exponent = int64(resArrI[2].(uint64))
							default:
								log.Println("@ERR SIZE 3! NOT INT/UINT IN MANTISSA: %s",
									reflect.TypeOf(resArrI[1]))
								return nil
							}
							resource["type"] = dataTypeToString(Decimal)
							resource["data"] = decimalToString(mantissa, exponent)
						} else if resType == String {
							switch resArrI[1].(type) {
							case string:
								resource["data"] = resArrI[1]
							case nil:
								resource["data"] = ""
							default:
								log.Printf("@ERR SIZE 3! NOT STRING: %s\n",
									reflect.TypeOf(resArrI[1]))
								return nil
							}

							resource["type"] = dataTypeToString(String)
							resource["lang"] = langToString(Lang(resArrI[2].(uint64)))
						}
					}

				//Other types are simply decoded from msgpack
				case string:
					resource["type"] = dataTypeToString(Uri)
					resource["data"] = resI
				case int64:
					resource["type"] = dataTypeToString(Integer)
					resource["data"] = resI
				case uint64:
					resource["type"] = dataTypeToString(Integer)
					resource["data"] = resI
				case bool:
					resource["type"] = dataTypeToString(Boolean)
					resource["data"] = resI
				default:
					log.Printf("@ERR! UNSUPPORTED TYPE %s\n", reflect.TypeOf(resI))
					return nil
				}
				resources = append(resources, resource)
			}
			individual[predicate] = resources
		}

		return individual
}
