package main

import (
	"bufio"
	"encoding/binary"
	"log"
	"reflect"
	"unsafe"
	"cbor"
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

func jeq(cborv interface{}) bool {
	switch i := cborv.(type) {
	case uint64:
		log.Printf("%d", i)
		return true
//	case big.Int:
		//		log.Printf("%s", i)
//		return true
	case int64:
		log.Printf("%s", i)
		return true
	case float32:
		log.Printf("%f", float64(i))
		return true
	case float64:
		log.Printf("%f", i)
		return true
	case bool:
		log.Printf("%b", i)
		return true
	case string:
		log.Printf("%s", i)
		return true
		
	case []interface{}:
	log.Print("array")
		for idx, cav := range i {
			log.Printf("array: idx=%d", idx)
			if !jeq(cav) {
				return false
			}
		}
		return true

	case nil:
		log.Printf("nil")

		return true
	case map[interface{}]interface{}:

		for key, cav := range i {
			log.Printf("map: key=%s", key)
			if !jeq(cav) {
				log.Print("map: false")
				return false
			}
		}
		return true
	case []byte:
		log.Printf("%s", string(i))
		return true		
	case cbor.CBORTag:
		//log.Printf("tag: %s, type=%s tag=%s", cborv, i, i.Tag)
		return jeq(i.WrappedObject)		
	case interface{}:
//		var tt cbor.CBORTag = cbor.CBORTag (cborv) 
		log.Printf("tag: %s, type=%s", cborv, i)
		return true
	default:
		log.Printf("default: %s, type=%s", cborv, i)
	}
	return false
}
