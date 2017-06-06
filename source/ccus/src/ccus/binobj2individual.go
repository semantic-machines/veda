package main

import (
	"cbor"
	"log"
)

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
