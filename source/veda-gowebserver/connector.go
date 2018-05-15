package main

/*
 #cgo CFLAGS: -I../authorization
 #cgo LDFLAGS: -L../lib64 -lauthorization
 #include <authorization.h>
*/
import "C"

import (
	"encoding/json"
	"github.com/itiu/lmdb-go/lmdb"
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
	
	Indv [](map[interface {}]interface{})
	
	as_indv bool
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
		err = conn.indivEnv.Open("./data/lmdb-individuals", lmdb.Readonly|lmdb.NoMetaSync|lmdb.NoSync|lmdb.NoLock, 0644)
		if err != nil {
			log.Fatal("ERR! can not open lmdb individuals base: ", err)
			conn.db_is_open = false
			return
		}

		err = conn.ticketEnv.Open("./data/lmdb-tickets", lmdb.Readonly|lmdb.NoMetaSync|lmdb.NoSync|lmdb.NoLock, 0644)
		if err != nil {
			log.Fatal("ERR! can not open tickets lmdb base: ", err)
			conn.db_is_open = false
			return
		}

		conn.db_is_open = true
	}
}

func (conn *Connector) reopen_individual_db() {
	var err error
	if conn.tt_client != nil {

	} else {

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
			log.Fatal("ERR! can not open lmdb individuals base: ", err)
			conn.db_is_open = false
			return
		}
	}
}

func (conn *Connector) reopen_ticket_db() {
	var err error

	if conn.tt_client != nil {

	} else {
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
			log.Fatal("ERR! can not open tickets lmdb base: ", err)
			conn.db_is_open = false
			return
		}
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

	if conn.tt_client != nil {

		for i := 0; i < len(uris); i++ {
			resp, err := conn.tt_client.Select("individuals", "primary", 0, 1, tarantool.IterEq, []interface{}{uris[i]})
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
						rr.Data = append(rr.Data, tpl[1].(string))
					}
				}

			}
		}
		rr.CommonRC = Ok

	} else {
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

	if conn.tt_client != nil {

/*
 		var resp []interface{}
		
		err := conn.tt_client.SelectTyped("tickets", "primary", 0, 1, tarantool.IterEq, tarantool.StringKey{ticketIDs[0]}, &resp)
			log.Printf("resp=%v\n", resp)
		if err != nil {
			log.Println("Error:", err)
		} else {
				rr.OpRC = append(rr.OpRC, Ok)
				//rr.Data = append(rr.Data, resp[1].(string))
				//rr.Indv = append(rr.Indv, tpl[1].(map[interface {}]interface {}))
				//rr.as_indv = true
				rr.CommonRC = Ok
		}
*/ 

		resp, err := conn.tt_client.Select("tickets", "primary", 0, 1, tarantool.IterEq, []interface{}{ticketIDs[0]})
		if err != nil {
			log.Println("Error", err)
		} else {
			if tpl, ok := resp.Data[0].([]interface{}); !ok {
				log.Println("Unexpected body of Insert")
				rr.CommonRC = InternalServerError
			} else {
				rr.OpRC = append(rr.OpRC, Ok)
				rr.Data = append(rr.Data, tpl[1].(string))
				rr.CommonRC = Ok
			}
		}

	} else {
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
	}

	return rr
}
