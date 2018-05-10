package main

/*
 #cgo CFLAGS: -I../authorization
 #cgo LDFLAGS: -L../lib64 -lauthorization
 #include <authorization.h>
*/
import "C"

import (
	"bytes"
	"encoding/json"
	"log"
	"net"
	//"os"
	"bufio"
	"strings"
	"time"
	//"fmt"
	"github.com/itiu/lmdb-go/lmdb"
	"gopkg.in/vmihailenco/msgpack.v2"
	"unsafe"
)

//Connector represents struct for connection to tarantool
type Connector struct {
	//Tcp connection to tarantool
	conn net.Conn
	//Address of tarantool database
	addr string

	indivEnv  *lmdb.Env
	ticketEnv *lmdb.Env

	db_is_open bool
}

//RequestResponse represents structure for tarantool request response
type RequestResponse struct {
	//ResultCode for request
	CommonRC ResultCode
	//ResultCode for each uri in request
	OpRC []ResultCode
	//Response data
	Data []string
	//Returned rights for auth requests
	Rights []uint8
}

//MaxPacketSize is critical value for request/response packets,
//if size is bigger than error is returned
const MaxPacketSize = 1024 * 1024 * 10

const (
	//Put is request code for tarantool put
	Put = 1
	//Get is request code for tarantool get
	Get = 2
	//GetTicket is request code for tarantool get_ticket
	GetTicket = 3
	//Authorize is request code for tarantool authorize
	Authorize = 8
	//GetRightsOrigin is request code for get_rights_origin (authorize with aggregation)
	GetRightsOrigin = 9
	//GetMembership is request code for get_membership (authorize with aggregation)
	GetMembership = 10
	//Remove is request code for tarantool remove
	Remove = 51
)

func (conn *Connector) open_dbs() {
	var err error
	err = conn.indivEnv.Open("./data/lmdb-individuals", lmdb.Readonly|lmdb.NoMetaSync|lmdb.NoSync|lmdb.NoLock, 0644)
	if err != nil {
		log.Fatal("Err: can not open lmdb individuals base: ", err)
		conn.db_is_open = false
		return
	}

	err = conn.ticketEnv.Open("./data/lmdb-tickets", lmdb.Readonly|lmdb.NoMetaSync|lmdb.NoSync|lmdb.NoLock, 0644)
	if err != nil {
		log.Fatal("Err: can not open tickets lmdb base: ", err)
		conn.db_is_open = false
		return
	}

	conn.db_is_open = true
}

func (conn *Connector) reopen_individual_db() {
	var err error

	log.Println("INFO! reopen individual db")

	conn.indivEnv.Close()

	conn.indivEnv, err = lmdb.NewEnv()
	if err != nil {
		log.Fatal("@ERR CREATING INDIVIDUALS LMDB ENV")
	}

	err = conn.indivEnv.SetMaxDBs(1)
	if err != nil {
		log.Fatal("@ERR SETTING INDIVIDUALS MAX DBS ", err)
	}

	err = conn.indivEnv.Open("./data/lmdb-individuals", lmdb.Readonly|lmdb.NoMetaSync|lmdb.NoSync|lmdb.NoLock, 0644)
	if err != nil {
		log.Fatal("Err: can not open lmdb individuals base: ", err)
		conn.db_is_open = false
		return
	}
}

func (conn *Connector) reopen_ticket_db() {
	var err error

	conn.ticketEnv.Close()

	conn.ticketEnv, err = lmdb.NewEnv()
	if err != nil {
		log.Fatal("@ERR CREATING LMDB TICKETS ENV")
	}

	err = conn.ticketEnv.SetMaxDBs(1)
	if err != nil {
		log.Fatal("@ERR SETTING ID MAX TICKETS DBS ", err)
	}

	err = conn.ticketEnv.Open("./data/lmdb-tickets", lmdb.Readonly|lmdb.NoMetaSync|lmdb.NoSync|lmdb.NoLock, 0644)
	if err != nil {
		log.Fatal("Err: can not open tickets lmdb base: ", err)
		conn.db_is_open = false
		return
	}
}

//Connect tries to connect to socket in tarantool while connection is not established
func (conn *Connector) Connect(addr string) {
	var err error
	conn.indivEnv, err = lmdb.NewEnv()
	if err != nil {
		log.Fatal("@ERR CREATING INDIVIDUALS LMDB ENV")
	}

	err = conn.indivEnv.SetMaxDBs(1)
	if err != nil {
		log.Fatal("@ERR SETTING INDIVIDUALS MAX DBS ", err)
	}

	conn.ticketEnv, err = lmdb.NewEnv()
	if err != nil {
		log.Fatal("@ERR CREATING LMDB TICKETS ENV")
	}

	err = conn.ticketEnv.SetMaxDBs(1)
	if err != nil {
		log.Fatal("@ERR SETTING ID MAX TICKETS DBS ", err)
	}

	/*	var err error
		conn.addr = addr
		conn.conn, err = net.Dial("tcp", addr)

		for err != nil {
			time.Sleep(3000 * time.Millisecond)
			conn.conn, err = net.Dial("tcp", addr)
			log.Println("@TRY CONNECT")
		}*/
}

//doRequest encodes request  and sends it to tarantool, after that it reading and decoding response
func doRequest(needAuth bool, userUri string, data []string, trace, traceAuth bool, op uint) (ResultCode, []byte) {
	var request bytes.Buffer
	var response []byte

	writer := bufio.NewWriter(&request)
	encoder := msgpack.NewEncoder(writer)

	//for GetRightsOrigin, Authorize and GetMembership array
	//for requset size is bigget because of trace param
	if op == GetRightsOrigin || op == Authorize || op == GetMembership {
		encoder.EncodeArrayLen(len(data) + 4)
	} else {
		encoder.EncodeArrayLen(len(data) + 3)
	}
	// encoder.EncodeArrayLen(len(data) + 3)

	//Encoding operation code, needAuth flag, trace if needed and userUri
	encoder.EncodeUint(op)
	encoder.EncodeBool(needAuth)
	if op == GetRightsOrigin || op == Authorize || op == GetMembership {
		encoder.EncodeBool(traceAuth)
	}
	encoder.EncodeString(userUri)

	//Encode request data
	for i := 0; i < len(data); i++ {
		encoder.EncodeString(data[i])
	}

	writer.Flush()
	if trace {
		log.Println("@CONNECTOR GET DATA SIZE ", request.Len())
	}

	//Encoding request size for sending via socket
	requestSize := uint32(request.Len())
	buf := make([]byte, 4)
	buf[0] = byte((requestSize >> 24) & 0xFF)
	buf[1] = byte((requestSize >> 16) & 0xFF)
	buf[2] = byte((requestSize >> 8) & 0xFF)
	buf[3] = byte(requestSize & 0xFF)
	buf = append(buf, request.Bytes()...)

	//Retry sending until not successed
	//If error occured on some step try to reconnect and restart sending
	for {
		var responseSize uint32
		var n int
		var err error

		n, err = 0, nil
		//Writing until whole buffer is sent
		for n < len(buf) {
			var sent int
			sent, err = conn.conn.Write(buf[n:])
			if err != nil {
				break
			}
			n += sent
		}

		if err != nil {
			log.Printf("@ERR ON SEND OP %v: REQUEST %v\n", op, err)
			time.Sleep(3000 * time.Millisecond)
			conn.conn, err = net.Dial("tcp", conn.addr)
			log.Printf("@RECONNECT %v REQUEST\n", op)
			continue
		}

		if trace {
			log.Printf("@CONNECTOR OP %v: SEND %v", op, n)
		}

		buf = make([]byte, 4)
		n, err = 0, nil
		//Reading response size
		for n < 4 {
			var read int
			read, err = conn.conn.Read(buf[n:])
			if err != nil {
				break
			}
			n += read
		}

		if err != nil {
			log.Printf("@ERR OP %v: RECEIVING RESPONSE SIZE BUF %v\n", op, err)
			time.Sleep(3000 * time.Millisecond)
			conn.conn, err = net.Dial("tcp", conn.addr)
			log.Printf("@RECONNECT %v REQUEST\n", op)
			continue
		}

		if err != nil {
			log.Printf("@ERR OP %v: RECEIVING RESPONSE SIZE BUF %v\n", op, err)
		}

		//decoding response size
		for i := 0; i < 4; i++ {
			responseSize = (responseSize << 8) + uint32(buf[i])
		}

		if trace {
			log.Printf("@CONNECTOR OP %v: RESPONSE SIZE %v\n", op, responseSize)
		}

		//If response size is bigger than packet size than return SizeTooLarge to client
		if responseSize > MaxPacketSize {
			log.Printf("@ERR OP %v: RESPONSE IS TOO LARGE %v\n", op, data)
			return SizeTooLarge, nil
		}

		//Reading response until whole buffer is read
		response = make([]byte, responseSize)
		n, err = 0, nil
		for n < int(responseSize) {
			var read int
			read, err = conn.conn.Read(response[n:])
			if err != nil {
				break
			}
			n += read
		}

		if err != nil {
			log.Printf("@ERR ON READING RESPONSE OP %v: %v", op, err)
			time.Sleep(3000 * time.Millisecond)
			conn.conn, err = net.Dial("tcp", conn.addr)
			log.Printf("@RECONNECT %v REQUEST\n", op)
			continue
		}

		if trace {
			log.Printf("@CONNECTOR OP %v: RECEIVE RESPONSE %v\n", op, n)
		}

		if err != nil {
			log.Printf("@ERR RECEIVING OP %v: RESPONSE %v\n", op, err)
		}

		if trace {
			log.Printf("@CONNECTOR %v RECEIVED RESPONSE %v\n", op, string(response))
		}

		//If there are no errors than break cycle and return Ok code and response
		break
	}
	return Ok, response
}

//Put sends put request to tarantool, data here represented with msgpacks of individuals
func (conn *Connector) Put(needAuth bool, userUri string, individuals []string, trace bool) RequestResponse {
	var rr RequestResponse

	if conn.db_is_open == false {
		conn.open_dbs()
	}

	//If user uri is too shorth than return not authorized
	if len(userUri) < 3 {
		rr.CommonRC = NotAuthorized
		log.Println("@ERR CONNECTOR PUT: ", individuals)
		return rr
	}

	//If no individuals passed than return NoContent to client
	if len(individuals) == 0 {
		rr.CommonRC = NoContent
		return rr
	}

	if trace {
		log.Printf("@CONNECTOR PUT: PACK PUT REQUEST need_auth=%v, user_uri=%v, uris=%v \n",
			needAuth, userUri, individuals)
	}

	//Send request
	rcRequest, response := doRequest(needAuth, userUri, individuals, trace, false, Put)
	//If failed return fail code
	if rcRequest != Ok {
		rr.CommonRC = rcRequest
		return rr
	}

	//Decoding msgpack response
	decoder := msgpack.NewDecoder(bytes.NewReader(response))
	arrLen, _ := decoder.DecodeArrayLen()
	rc, _ := decoder.DecodeUint()
	//Decoding common code for request
	rr.CommonRC = ResultCode(rc)

	if trace {
		log.Println("@CONNECTOR PUT: COMMON RC ", rr.CommonRC)
	}

	rr.OpRC = make([]ResultCode, len(individuals))

	//For put request only operation codes returned
	for i := 1; i < arrLen; i++ {
		rc, _ = decoder.DecodeUint()
		rr.OpRC[i-1] = ResultCode(rc)
		if trace {
			log.Println("@CONNECTOR PUT: OP CODE ", rr.OpRC[i-1])
		}
	}

	return rr
}

//Get sends get request to tarantool, individuals uris passed as data here
func (conn *Connector) Get(needAuth bool, userUri string, uris []string, trace bool, reopen bool) RequestResponse {
	var rr RequestResponse

	if conn.db_is_open == false {
		conn.open_dbs()
	} else if reopen == true {
		conn.reopen_individual_db()
	}

	//If user uri is too short return NotAuthorized to client
	if len(userUri) < 3 {
		rr.CommonRC = NotAuthorized
		log.Println("@ERR CONNECTOR GET: ", uris)
		return rr
	}

	//If no uris passed than return NoContent to client
	if len(uris) == 0 {
		rr.CommonRC = NoContent
		return rr
	}

	rr.OpRC = make([]ResultCode, 0, len(uris))
	rr.Data = make([]string, 0, len(uris))
	err := conn.indivEnv.View(func(txn *lmdb.Txn) (err error) {
		dbi, err := txn.OpenDBI("", 0)
		if err != nil {
			return err
		}
		for i := 0; i < len(uris); i++ {
			val, err := txn.Get(dbi, []byte(uris[i]))
			if err == lmdb.NotFound {
				rr.OpRC = append(rr.OpRC, NotFound)
				continue
			} else if err != nil {
				return err
			}

			if needAuth {

				curi := C.CString(uris[i])
				defer C.free(unsafe.Pointer(curi))

				cuser_uri := C.CString(userUri)
				defer C.free(unsafe.Pointer(cuser_uri))

				if C.authorize_r(curi, cuser_uri, 2, reopen) != 2 {
					rr.OpRC = append(rr.OpRC, NotAuthorized)
					continue
				}
			}

			rr.OpRC = append(rr.OpRC, Ok)
			rr.Data = append(rr.Data, string(val))
		}
		return nil
	})

	rr.CommonRC = Ok
	if err != nil {
		if lmdb.IsErrno(err, lmdb.NotFound) == true {
			rr.CommonRC = NotFound
		} else {
			if lmdb.IsErrno(err, lmdb.MapResized) == true {
				conn.reopen_individual_db()
				return conn.Get(needAuth, userUri, uris, trace, reopen)
			} else if lmdb.IsErrno(err, lmdb.BadRSlot) == true {
				return conn.Get(needAuth, userUri, uris, trace, reopen)
			}

			log.Printf("ERR! Get: GET INDIVIDUAL FROM LMDB %v, keys=%s\n", err, uris)
			rr.CommonRC = InternalServerError
		}
	}

	return rr
}

func strRightToByte(strRight string) uint8 {
	right := uint8(0)

	if strings.Contains(strRight, "C") {
		right |= AccessCanCreate
	}
	if strings.Contains(strRight, "R") {
		right |= AccessCanRead
	}
	if strings.Contains(strRight, "U") {
		right |= AccessCanUpdate
	}
	if strings.Contains(strRight, "D") {
		right |= AccessCanDelete
	}

	return right
}

func trace_acl(str *C.char) {

}

//Authorize sends authorize, get membership or get rights origin request,
//it depends on parametr operation. Individuals uris passed as data here
func (conn *Connector) Authorize(needAuth bool, userUri string, uri string, operation uint,
	trace, traceAuth bool) RequestResponse {
	var rr RequestResponse

	//If userUri is too short return NotAuthorized to client
	if len(userUri) < 3 {
		rr.CommonRC = NotAuthorized
		log.Println("@ERR CONNECTOR AUTHORIZE: ", uri)
		return rr
	}

	//If no uris passed than NoContent returned to client.
	if len(uri) == 0 {
		rr.CommonRC = NoContent
		return rr
	}

	if trace {
		log.Printf("@CONNECTOR AUTHORIZE: PACK AUTHORIZE REQUEST need_auth=%v, user_uri=%v, uri=%v \n",
			needAuth, userUri, uri)
	}

	if operation == Authorize {

		rr.Rights = make([]uint8, 1)
		rr.OpRC = make([]ResultCode, 1)

		curi := C.CString(uri)
		defer C.free(unsafe.Pointer(curi))

		cuser_uri := C.CString(userUri)
		defer C.free(unsafe.Pointer(cuser_uri))

		right := C.authorize_r(curi, cuser_uri, 15, false)

		rr.Rights[0] = uint8(right)
		rr.OpRC[0] = Ok

		rr.CommonRC = Ok
	}

	if operation == GetRightsOrigin {

		curi := C.CString(uri)
		defer C.free(unsafe.Pointer(curi))

		cuser_uri := C.CString(userUri)
		defer C.free(unsafe.Pointer(cuser_uri))

		rights_str := C.GoString(C.get_trace(curi, cuser_uri, 15, C.TRACE_ACL, true))
		//defer C.free(unsafe.Pointer(right))

		rr.Rights = make([]uint8, 1)
		rr.Data = make([]string, 1)
		rr.OpRC = make([]ResultCode, 1)

		statements := strings.Split(rights_str, "\n")

		data := make([]interface{}, 0)
		for j := 0; j < len(statements)-1; j++ {

			parts := strings.Split(statements[j], ";")
			statementIndiv := map[string]interface{}{
				"@": "_",
				"rdf:type": []interface{}{
					map[string]interface{}{"type": "Uri", "data": "v-s:PermissionStatement"},
				},
				"v-s:permissionSubject": []interface{}{
					map[string]interface{}{"type": "Uri", "data": parts[1]},
				},
				"v-s:permissionObject": []interface{}{
					map[string]interface{}{"type": "Uri", "data": parts[0]},
				},
				parts[2]: []interface{}{
					map[string]interface{}{"type": "Boolean", "data": true},
				},
			}
			data = append(data, statementIndiv)
		}

		//			commentIndiv := map[string]interface{}{
		//				"@": "_",
		//				"rdf:type": []interface{}{
		//					map[string]interface{}{"type": "Uri", "data": "v-s:PermissionStatement"},
		//				},
		//				"v-s:permissionSubject": []interface{}{
		//					map[string]interface{}{"type": "Uri", "data": "?"},
		//				},

		//				"rdfs:comment": []interface{}{
		//					map[string]interface{}{"type": "String", "lang": "NONE", "data": rights[i+2]},
		//				},
		//			}
		//			data = append(data, commentIndiv)

		jsonBytes, _ := json.Marshal(data)
		rr.Data[0] = string(jsonBytes)
		rr.OpRC[0] = Ok

		rr.CommonRC = Ok
	}

	if operation == GetMembership {

		curi := C.CString(uri)
		defer C.free(unsafe.Pointer(curi))

		cuser_uri := C.CString(userUri)
		defer C.free(unsafe.Pointer(cuser_uri))

		info_str := C.GoString(C.get_trace(curi, cuser_uri, 15, C.TRACE_GROUP, true))

		rr.Rights = make([]uint8, 1)
		rr.Data = make([]string, 1)
		rr.OpRC = make([]ResultCode, 1)

		parts := strings.Split(info_str, "\n")

		memberOf := make([]interface{}, len(parts)-1)
		for k := 0; k < len(parts)-1; k++ {
			memberOf[k] = map[string]interface{}{"type": "Uri", "data": parts[k]}
		}

		membershipIndividual := map[string]interface{}{
			"@": "_",
			"rdf:type": []interface{}{
				map[string]interface{}{"type": "Uri", "data": "v-s:Membership"},
			},
			"v-s:resource": []interface{}{
				map[string]interface{}{"type": "Uri", "data": uri},
			},
			"v-s:memberOf": memberOf,
		}

		jsonBytes, _ := json.Marshal(membershipIndividual)
		rr.Data[0] = string(jsonBytes)
		rr.OpRC[0] = Ok

		rr.CommonRC = Ok
	}

	return rr
}

//GetTicket sends get ticket request to tarantool, ticket ids here passes as data
func (conn *Connector) GetTicket(ticketIDs []string, trace bool) RequestResponse {
	var rr RequestResponse

	if conn.db_is_open == false {
		conn.open_dbs()
	}

	//If no ticket ids passed than NoContent returned to client.
	if len(ticketIDs) == 0 {
		rr.CommonRC = NoContent
		return rr
	}

	rr.OpRC = make([]ResultCode, 0, len(ticketIDs))
	rr.Data = make([]string, 0, len(ticketIDs))
	err := conn.ticketEnv.View(func(txn *lmdb.Txn) (err error) {
		dbi, err := txn.OpenDBI("", 0)
		if err != nil {
			return err
		}
		for _, id := range ticketIDs {
			val, err := txn.Get(dbi, []byte(id))
			if err == lmdb.NotFound {
				rr.OpRC = append(rr.OpRC, NotFound)
				continue
			} else if err != nil {
				return err
			}

			rr.OpRC = append(rr.OpRC, Ok)
			rr.Data = append(rr.Data, string(val))
		}
		return nil
	})

	rr.CommonRC = Ok
	if err != nil {
		if lmdb.IsErrno(err, lmdb.MapResized) == true {
			conn.reopen_ticket_db()
			return conn.GetTicket(ticketIDs, trace)
		} else if lmdb.IsErrno(err, lmdb.BadRSlot) == true {
			return conn.GetTicket(ticketIDs, trace)
		}
		log.Printf("ERR! GetTicket: GET INDIVIDUAL FROM LMDB, err=%s\n", err)
		rr.CommonRC = InternalServerError
	}

	return rr
}
