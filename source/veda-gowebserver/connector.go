package main

/*
 #cgo CFLAGS: -I../authorization
 #cgo LDFLAGS: -L../lib64 -lauthorization
 #include <authorization.h>
*/
import "C"

import (
	//"encoding/json"
	"github.com/op/go-nanomsg"
	"github.com/tarantool/go-tarantool"
	"log"
	"strings"
	"time"
	"unsafe"
)

//Connector represents struct for connection to tarantool
type Connector struct {
	//Address of tarantool database
	tt_addr   string
	tt_client *tarantool.Connection

	lmdb_client *nanomsg.Socket

	db_is_open bool
}

const (
	TT   = 1
	LMDB = 2
	NONE = 0
)

//RequestResponse represents structure for tarantool request response
type RequestResponse struct {
	//ResultCode for request
	CommonRC ResultCode
	//ResultCode for each uri in request
	OpRC []ResultCode

	//Returned rights for auth requests
	Rights []uint8

	indv []Individual

	uris        []string
	data_binobj []string
	data_obj_tt []map[interface{}]interface{}
	src_type    int
}

func (rr *RequestResponse) SetUris(data []string) {
	rr.uris = data
}

func (rr *RequestResponse) AddIndv(data Individual) {
	rr.src_type = NONE
	rr.indv = append(rr.indv, data)
}

func (rr *RequestResponse) AddTTResult(data map[interface{}]interface{}) {
	rr.src_type = TT
	rr.data_obj_tt = append(rr.data_obj_tt, data)
}

func (rr *RequestResponse) AddLMDBResult(data string) {
	rr.src_type = LMDB
	rr.data_binobj = append(rr.data_binobj, data)
}

func (rr *RequestResponse) GetCount() int {
	if rr.src_type == TT {
		return len(rr.data_obj_tt)
	} else if rr.src_type == LMDB {
		return len(rr.data_binobj)
	} else {
		return len(rr.indv)
	}
}

func (rr *RequestResponse) GetIndv(idx int) Individual {
	if len(rr.indv) == 0 {
		rr.indv = make([]Individual, len(rr.uris))
	}

	if rr.indv[idx] != nil {
		return rr.indv[idx]
	} else {
		if rr.src_type == TT {
			rr.indv[idx] = ttResordToMap(rr.uris[idx], rr.data_obj_tt[idx])
		} else if rr.src_type == LMDB {
			rr.indv[idx] = BinobjToMap(string(rr.data_binobj[idx]))
		}
		return rr.indv[idx]
	}
}

func (rr *RequestResponse) GetIndvs() []Individual {
	if len(rr.indv) == 0 {
		rr.indv = make([]Individual, len(rr.uris))
	}

	for idx := 0; idx < len(rr.uris); idx++ {
		if rr.indv[idx] == nil {
			if rr.src_type == TT {
				rr.indv[idx] = ttResordToMap(rr.uris[idx], rr.data_obj_tt[idx])
			} else if rr.src_type == LMDB {
				rr.indv[idx] = BinobjToMap(string(rr.data_binobj[idx]))
			}
		}
	}

	return rr.indv
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
	if conn.tt_client != nil {
		resp, err := conn.tt_client.Ping()

		if err != nil {
			conn.db_is_open = false
			log.Fatal(err)
		} else {
			conn.db_is_open = true
			log.Println(resp.Code)
			log.Println(resp.Data)
		}

	} else {

	}
}

func (conn *Connector) reopen_individual_db() {
	//var err error
	if conn.tt_client != nil {

	} else {

	}
}

func (conn *Connector) reopen_ticket_db() {
	//var err error

	if conn.tt_client != nil {

	} else {
	}
}

//Connect tries to connect to socket in tarantool while connection is not established
func (conn *Connector) Connect(tt_addr string) {
	var err error

	if tt_addr != "" {
		opts := tarantool.Opts{User: "guest"}

		conn.tt_addr = tt_addr
		tt_client, err := tarantool.Connect(tt_addr, opts)

		for err != nil {
			log.Println("ERR! Creating tarantool connection: err=", err)
			log.Println("INFO! sleep")
			time.Sleep(3000 * time.Millisecond)
			log.Println("INFO! retry connect")
			tt_client, err = tarantool.Connect(tt_addr, opts)
		}

		log.Println("INFO! tarantool connect is ok")
		conn.tt_client = tt_client
	} else {

		conn.lmdb_client, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
		if err != nil {
			conn.db_is_open = false
			log.Fatal("ERR! open_dbs: nanomsg.NewSocket")

		} else {
			conn.db_is_open = true
		}

		//log.Println("use lmdb service url: ", lmdbServiceURL)
		_, err = conn.lmdb_client.Connect(lmdbServiceURL)
		for err != nil {
			log.Println("ERR! Creating lmdb service connection: err=", err)
			log.Println("INFO! sleep")
			time.Sleep(3000 * time.Millisecond)
			log.Println("INFO! retry connect")
			_, err = conn.lmdb_client.Connect(queryServiceURL)
		}
	}
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
		log.Println("ERR! CONNECTOR GET: ", uris)
		return rr
	}

	//If no uris passed than return NoContent to client
	if len(uris) == 0 {
		rr.CommonRC = NoContent
		return rr
	}

	rr.OpRC = make([]ResultCode, 0, len(uris))
	rr.SetUris(uris)
	//rr.Indv = make([]Individual, 0, len(uris))

	if conn.tt_client != nil {

		for i := 0; i < len(uris); i++ {
			resp, err := conn.tt_client.Select("INDIVIDUALS", "primary", 0, 1, tarantool.IterEq, []interface{}{uris[i]})
			if err != nil {
				log.Println("Error", err)
			} else {
				if len(resp.Data) == 0 {
					rr.OpRC = append(rr.OpRC, NotFound)
					continue
				}
				if tpl, ok := resp.Data[0].([]interface{}); !ok {
					log.Println("Unexpected body of Insert")
					rr.CommonRC = InternalServerError
				} else {
					if len(tpl) == 2 {
						if needAuth {
							curi := C.CString(uris[i])
							defer C.free(unsafe.Pointer(curi))
							cuser_uri := C.CString(userUri)
							defer C.free(unsafe.Pointer(cuser_uri))
							if reopen == true {
								if C.authorize_r(curi, cuser_uri, 2, true) != 2 {
									rr.OpRC = append(rr.OpRC, NotAuthorized)
									continue
								}
							} else {
								if C.authorize_r(curi, cuser_uri, 2, false) != 2 {
									rr.OpRC = append(rr.OpRC, NotAuthorized)
									continue
								}
							}
						}
						rr.OpRC = append(rr.OpRC, Ok)
						rr.AddTTResult(tpl[1].(map[interface{}]interface{}))
						//rr.Indv = append(rr.Indv, ttResordToMap(uris[i], tpl[1].(map[interface{}]interface{})))

						//						ii := tpl[1].(string)
						//						rr.Indv = append(rr.Indv, BinobjToMap(ii))
					}
				}

			}
		}
		rr.CommonRC = Ok

	} else {
		for i := 0; i < len(uris); i++ {
			request := "I," + uris[i]
			_, err := conn.lmdb_client.Send([]byte(request), 0)
			if err != nil {
				log.Println("ERR! send to lmdb service", err)
				rr.CommonRC = InternalServerError
				return rr
			} else {
				responseBuf, err := conn.lmdb_client.Recv(0)
				if err != nil {
					log.Println("ERR! recv from lmdb service", err)
					rr.CommonRC = InternalServerError
					return rr
				} else {

					val := string(responseBuf)
					if val == "[]" {
						rr.OpRC = append(rr.OpRC, NotFound)
						continue
					}

					if strings.Index(val, "ERR") > 0 {
						rr.CommonRC = InternalServerError
						return rr
					}

					if needAuth {
						curi := C.CString(uris[i])
						defer C.free(unsafe.Pointer(curi))
						cuser_uri := C.CString(userUri)
						defer C.free(unsafe.Pointer(cuser_uri))
						if reopen == true {
							if C.authorize_r(curi, cuser_uri, 2, true) != 2 {
								rr.OpRC = append(rr.OpRC, NotAuthorized)
								continue
							}
						} else {
							if C.authorize_r(curi, cuser_uri, 2, false) != 2 {
								rr.OpRC = append(rr.OpRC, NotAuthorized)
								continue
							}
						}
					}

					rr.OpRC = append(rr.OpRC, Ok)
					rr.AddLMDBResult(string(responseBuf))
					//rr.Indv = append(rr.Indv, BinobjToMap(string(responseBuf)))

				}
			}
		}
		rr.CommonRC = Ok
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
		log.Println("ERR! CONNECTOR AUTHORIZE: ", uri)
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
		rr.OpRC = make([]ResultCode, 1)

		statements := strings.Split(rights_str, "\n")
		//rr.Indv = make([]Individual, 0)

		for j := 0; j < len(statements)-1; j++ {

			parts := strings.Split(statements[j], ";")
			statementIndiv := Individual{
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
			rr.AddIndv(statementIndiv)
			//rr.Indv = append(rr.Indv, statementIndiv)
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
		//rr.Indv = make([]Individual, 1)
		rr.OpRC = make([]ResultCode, 1)

		parts := strings.Split(info_str, "\n")

		memberOf := make([]interface{}, len(parts)-1)
		for k := 0; k < len(parts)-1; k++ {
			memberOf[k] = map[string]interface{}{"type": "Uri", "data": parts[k]}
		}

		membershipIndividual := Individual{
			"@": "_",
			"rdf:type": []interface{}{
				map[string]interface{}{"type": "Uri", "data": "v-s:Membership"},
			},
			"v-s:resource": []interface{}{
				map[string]interface{}{"type": "Uri", "data": uri},
			},
			"v-s:memberOf": memberOf,
		}

		//rr.Indv[0] = membershipIndividual
		rr.AddIndv(membershipIndividual)
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
	//rr.Indv = make([]Individual, 0, len(ticketIDs))
	rr.SetUris(ticketIDs)

	if conn.tt_client != nil {

		resp, err := conn.tt_client.Select("TICKETS", "primary", 0, 1, tarantool.IterEq, []interface{}{ticketIDs[0]})
		if err != nil {
			log.Println("Error", err)
		} else {
			if len(resp.Data) == 0 {
				log.Println("ERR! Empty body of Insert")
				rr.CommonRC = InternalServerError
			} else if tpl, ok := resp.Data[0].([]interface{}); !ok {
				log.Println("ERR! Unexpected body of Insert")
				rr.CommonRC = InternalServerError
			} else {
				rr.OpRC = append(rr.OpRC, Ok)

				rr.AddTTResult(tpl[1].(map[interface{}]interface{}))
				//rr.Indv = append(rr.Indv, ttResordToMap(ticketIDs[0], tpl[1].(map[interface{}]interface{})))

				//ii := tpl[1].(string)
				//rr.Indv = append(rr.Indv, BinobjToMap(ii))
				rr.CommonRC = Ok
			}
		}

	} else {
		request := "T," + ticketIDs[0]
		_, err := conn.lmdb_client.Send([]byte(request), 0)
		if err != nil {
			log.Println("ERR! send to lmdb service", err)
			rr.CommonRC = InternalServerError
			return rr
		} else {
			responseBuf, err := conn.lmdb_client.Recv(0)
			if err != nil {
				log.Println("ERR! recv from lmdb service", err)
				rr.CommonRC = InternalServerError
				return rr
			} else {

				val := string(responseBuf)
				if val == "[]" {
					rr.OpRC = append(rr.OpRC, NotFound)
					return rr
				}

				if strings.Index(val, "ERR") > 0 {
					rr.CommonRC = InternalServerError
					return rr
				}

				rr.OpRC = append(rr.OpRC, Ok)
				rr.AddLMDBResult(string(responseBuf))
				//rr.Indv = append(rr.Indv, BinobjToMap(string(responseBuf)))
				rr.CommonRC = Ok
			}
		}
	}

	return rr
}

func NmCSend(s *nanomsg.Socket, data []byte, flags int) (int, error) {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("ERR! fail send to NN socket")
			return
		}
	}()

	tmp := make([]byte, len(data))
	copy(tmp, data)

	return s.Send(tmp, flags)
}

func NmSend(s *nanomsg.Socket, data []byte, flags int) (int, error) {
	defer func() {
		if r := recover(); r != nil {
			log.Printf("ERR! fail send to NN socket")
			return
		}
	}()

	return s.Send(data, flags)
}
