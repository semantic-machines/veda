package main

import (
	"bufio"
	"cbor"
	"encoding/binary"
	"log"
	"reflect"
	"unsafe"
)

func ulong_to_buff(_buff []uint8, pos int, data uint64) {
	_buff[pos+0] = uint8((data & 0x00000000000000FF))
	_buff[pos+1] = uint8((data & 0x000000000000FF00) >> 8)
	_buff[pos+2] = uint8((data & 0x0000000000FF0000) >> 16)
	_buff[pos+3] = uint8((data & 0x00000000FF000000) >> 24)
	_buff[pos+4] = uint8((data & 0x000000FF00000000) >> 32)
	_buff[pos+5] = uint8((data & 0x0000FF0000000000) >> 40)
	_buff[pos+6] = uint8((data & 0x00FF000000000000) >> 48)
	_buff[pos+7] = uint8((data & 0xFF00000000000000) >> 56)
}

func uint_to_buff(_buff []uint8, pos int, data uint32) {
	_buff[pos+0] = uint8((data & 0x000000FF))
	_buff[pos+1] = uint8((data & 0x0000FF00) >> 8)
	_buff[pos+2] = uint8((data & 0x00FF0000) >> 16)
	_buff[pos+3] = uint8((data & 0xFF000000) >> 24)
}

func uint_from_buff(buff []uint8, pos int) uint32 {
	num := binary.LittleEndian.Uint32(buff[pos : pos+8])
	return uint32(num)
}

func ulong_from_buff(buff []uint8, pos int) uint64 {
	num := binary.LittleEndian.Uint64(buff[pos : pos+8])
	return num
}

// Readln returns a single line (without the ending \n)
// from the input buffered reader.
// An error is returned iff there is an error with the
// buffered reader.
func Readln(r *bufio.Reader) (string, error) {
	var (
		isPrefix bool  = true
		err      error = nil
		line, ln []byte
	)
	for isPrefix && err == nil {
		line, isPrefix, err = r.ReadLine()
		ln = append(ln, line...)
	}
	return string(ln), err
}

func CopyString(s string) string {
	var b []byte
	h := (*reflect.SliceHeader)(unsafe.Pointer(&b))
	h.Data = (*reflect.StringHeader)(unsafe.Pointer(&s)).Data
	h.Len = len(s)
	h.Cap = len(s)
	return string(b)
}

func cbor2individual(individual *Individual, cborv interface{}) bool {
	//log.Print("cbor2individual #1")
	res, _ := read_element(individual, cborv, "", "")
	//log.Print("cbor2individual #E")
	return res
}

func read_element(individual *Individual, cborv interface{}, subject_uri string, predicate_uri string) (bool, string) {
	var new_subject_uri string

	switch i := cborv.(type) {
	case uint64:
		var resources Resources = individual.resources[predicate_uri]
		resources = append(resources, *NewResource_uint64(i))
		individual.resources[predicate_uri] = resources
		return true, ""
		//	case big.Int:
		//		log.Printf("%s", i)
		//		return true
	case int64:
		var resources Resources = individual.resources[predicate_uri]
		resources = append(resources, *NewResource_int64(i))
		individual.resources[predicate_uri] = resources
		return true, ""
	case float32:
		log.Printf("float32 %f", float64(i))
		return true, ""
	case float64:
		log.Printf("float64 %f", i)
		return true, ""
	case bool:
		log.Printf("bool %b", i)
		return true, ""
	case string:
		//log.Printf("string %s", i)
		return true, i

	case []interface{}:
		log.Print("array")
		for idx, cav := range i {
			log.Printf("array: idx=%d", idx)

			res, _ := read_element(individual, cav, subject_uri, predicate_uri)
			if !res {
				return false, ""
			}
		}
		return true, ""

	case nil:
		log.Printf("nil")

		return true, ""
	case map[interface{}]interface{}:

		for _key, cav := range i {
			var key string = _key.(string)
			if key == "@" {
				res, val := read_element(individual, cav, new_subject_uri, key)
				if !res {
					log.Print("map: false")
					return false, ""
				}
				individual.uri = val
				new_subject_uri = val
			}
		}

		for _key, cav := range i {
			//log.Printf("map: key=%s", _key)

			var key string = _key.(string)

			res, _ := read_element(individual, cav, new_subject_uri, key)
			if !res {
				log.Print("map: false")
				return false, ""
			}
		}
		return true, ""
	case []byte:
		log.Printf("byte %s", string(i))
		return true, ""
	case cbor.CBORTag:
		//log.Printf("tag: %s, type=%s tag=%s", cborv, i, i.Tag)
		res, val := read_element(individual, i.WrappedObject, "", "")
		tag := TAG(i.Tag)
		//		log.Printf("tag #1 subject_uri=%s, predicate_uri=%s", subject_uri, predicate_uri)
		if subject_uri != "" && predicate_uri != "" {

			var resources Resources = individual.resources[predicate_uri]

			if tag == TAG_TEXT_RU {
				resources = append(resources, *NewResource_value_lang(val, LANG_RU))
			} else if tag == TAG_TEXT_RU {
				resources = append(resources, *NewResource_value_lang(val, LANG_EN))
			} else if tag == TAG_URI {
				resources = append(resources, *NewResource_type_value(Uri, val))
			} else {
				resources = append(resources, *NewResource_type_value(String, val))
			}
			individual.resources[predicate_uri] = resources
		}

		return res, ""
	case interface{}:
		//		var tt cbor.CBORTag = cbor.CBORTag (cborv)
		log.Printf("interface: %s, type=%s", cborv, i)
		return true, ""
	default:
		log.Printf("default: %s, type=%s", cborv, i)
	}
	return false, ""
}
