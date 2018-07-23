package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/op/go-nanomsg"
	"github.com/valyala/fasthttp"
)

//ResultCode is type for representation of http codes
type ResultCode uint32

const (
	//Ok is success code
	Ok ResultCode = 200
	//BadRequest is returned for requests with invalid data
	BadRequest ResultCode = 400
	//NotAuthorized is returned than user doesn't have enough rights for do request
	NotAuthorized ResultCode = 472
	//NotFound is returned if individual not found
	NotFound ResultCode = 404
	//InternalServerError is returned if error occured during request handling
	InternalServerError ResultCode = 500
	//TicketExpired return if given ticket is expired
	TicketExpired ResultCode = 471
	//NoContent is returned if http-request has empty data
	NoContent ResultCode = 204
	//SizeTooLarge is returned if size of request or response is too large
	SizeTooLarge ResultCode = 1118
	//UnprocessableEntity is returned if individual not found
	UnprocessableEntity ResultCode = 422
	//Invalid identifier is returned if invalid queue identifier was passed
	InvalidIdentifier ResultCode = 904
)

type ticket struct {
	Id        string
	UserURI   string
	result    ResultCode
	StartTime int64
	EndTime   int64
}

const (
	tdbPath = "./data/trails/	"
)

//ticketCache is map to cache requested earlier tickets
var ticketCache map[string]ticket
var ticketCacheMutex = sync.RWMutex{}

//ontologyCache is map to cache requested earlier individuals from ontology
var ontologyCache map[string]Individual

//mifCache is map to cache opened ModuleInfoFile structs
var mifCache map[int]*ModuleInfoFile

//conn is Connector to trarantool database
var conn Connector

//socket is nanomsg socket connected to server
var g_mstorage_ch *nanomsg.Socket

//var mstorage_ch_Mutex = sync.RWMutex{}

//endpoint is nanomsg endpoint connected to server
//var endpoint *nanomsg.Endpoint

//aclSocket is nanomsg socket connected to acl service
//var aclSocket *nanomsg.Socket

//aclEndpoint is nanomsg endpoint connected to acl service
//var aclEndpoint *nanomsg.Endpoint

//querySocket is nanomsg socket connected to query service
//var querySocket *nanomsg.Socket

//aclEndpoint is nanomsg endpoint connected to acl service
//var queryEndpoint *nanomsg.Endpoint

//mainModuleURL is tcp address of veda server
var mainModuleURL = "tcp://127.0.0.1:9112"
var notifyChannelURL = "tcp://127.0.0.1:9111"
var queryServiceURL = "tcp://127.0.0.1:23000"
var lmdbServiceURL = "tcp://127.0.0.1:23001"
var tarantoolURL = "" // = "127.0.0.1:3309"
var webserverPort = "8080"
var webserverHTTPSPort = "8020"

//var aclServiceURL = "tcp://127.0.0.1:22000"
var useHTTPS = false

//attachmentsPath is path where files from request are stored
var attachmentsPath = "./data/files/"

//areExternalUsers is variable to activate ExternalUsers features
var areExternalUsers = false

//externalUsersTicketId is map to stoer external users tickets
//var externalUsersTicketId map[string]bool

//cons is connection to traildb
//var cons *tdb.TrailDBConstructor
var isTrail = true

//countTrails is variable to count trail requests, after limit they are flushed
var countTrails = 0

//portStr is string with port number vor fasthttp
var portStr = "8080"

// string BASE64_START_POS = "base64";

//codeToJsonException converts ResultCode value to its string representation
func codeToJsonException(code ResultCode) []byte {
	exception := make(map[string]interface{})

	switch code {
	case Ok:
		exception["statusMessage"] = "Ok"
	case BadRequest:
		exception["statusMessage"] = "BadRequest"
	case NotAuthorized:
		exception["statusMessage"] = "NotAuthorized"
	case NotFound:
		exception["statusMessage"] = "NotFound"
	case InternalServerError:
		exception["statusMessage"] = "InternalServerError"
	case TicketExpired:
		exception["statusMessage"] = "TicketExpired"
	case NoContent:
		exception["statusMessage"] = "NoContent"
	case SizeTooLarge:
		exception["statusMessage"] = "SizeToLarge"
	case UnprocessableEntity:
		exception["statusMessage"] = "UnprocessableEntity"
	default:
		exception["statusMessage"] = "UnknownError"
	}

	exceptionJSON, _ := json.Marshal(exception)
	return exceptionJSON
}

//requestHandler passes request context pointer to handler according to request pass
func requestHandler(ctx *fasthttp.RequestCtx) {
	routeParts := strings.Split(string(ctx.Path()[:]), "/")
	if len(routeParts) >= 2 && routeParts[1] == "files" {
		//log.Printf("@len=%v arr=%v\n", len(routeParts), routeParts)
		files(ctx, routeParts)
		return
	}

	switch string(ctx.Path()[:]) {
	case "/get_individual":
		getIndividual(ctx)
	case "/get_individuals":
		getIndividuals(ctx)

	case "/put_individual":
		putIndividual(ctx)
	case "/put_individuals":
		putIndividuals(ctx)

	case "/remove_individual":
		removeIndividual(ctx)
	case "/remove_from_individual":
		removeFromIndividual(ctx)

	case "/set_in_individual":
		setInIndividual(ctx)

	case "/add_to_individual":
		addToIndividual(ctx)

	case "/authenticate":
		authenticate(ctx)

	case "/get_rights":
		getRights(ctx)
	case "/get_rights_origin":
		getAclData(ctx, GetRightsOrigin)
	case "/get_membership":
		getAclData(ctx, GetMembership)

	case "/get_ticket_trusted":
		getTicketTrusted(ctx)
	case "/is_ticket_valid":
		isTicketValid(ctx)

	case "/query":
		query(ctx)

	case "/send_to_module":
		sendToModule(ctx)

	case "/get_operation_state":
		getOperationState(ctx)
	case "/flush":
		break

	//for tests request only sending file is needed
	case "/tests":
		ctx.SendFile("public/tests.html")
	default:

		fs := &fasthttp.FS{
			Root:       "public/",
			IndexNames: []string{"index.html"},
			Compress:   false,
		}
		fsHandler := fs.NewRequestHandler()
		fsHandler(ctx)
		//fasthttp.FSHandler("public/", 0)(ctx)
	}
}

func main() {

	log.SetFlags(log.LstdFlags | log.Lmicroseconds)

	var err error

	configWebServer()

	args := os.Args[1:]

	opt_external_users_http_port := ""

	for _, arg := range args {
		cuts := strings.Split(arg, "=")
		if len(cuts) == 2 {
			name := cuts[0]
			val := cuts[1]

			if name == "--http_port" {
				webserverPort = val
				fmt.Println("use command line param http_port=", webserverPort)
			} else if name == "--ext_usr_http_port" {
				opt_external_users_http_port = val
			}
		}
	}

	if opt_external_users_http_port != "" && opt_external_users_http_port == webserverPort {
		fmt.Println("use external user mode")
		areExternalUsers = true
	}

	g_mstorage_ch, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
	if err != nil {
		log.Fatal("ERR! ON CREATING SOCKET")
	}

	_, err = g_mstorage_ch.Connect(mainModuleURL)
	for err != nil {
		_, err = g_mstorage_ch.Connect(mainModuleURL)
		time.Sleep(3000 * time.Millisecond)
	}

	//	aclSocket, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
	//	if err != nil {
	//		log.Fatal("ERR! ON CREATING ACL SOCKET")
	//	}

	//	aclEndpoint, err = aclSocket.Connect(aclServiceURL)
	//	for err != nil {
	//		endpoint, err = aclSocket.Connect(aclServiceURL)
	//		time.Sleep(3000 * time.Millisecond)
	//	}
	/*
		querySocket, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
		if err != nil {
			log.Fatal("ERR! ON CREATING QUERY SOCKET")
		}

		log.Println("use query service url: ", queryServiceURL)
		queryEndpoint, err = querySocket.Connect(queryServiceURL)
		for err != nil {
			//		endpoint, err = aclSocket.Connect(aclServiceURL)
			//		time.Sleep(3000 * time.Millisecond)
		}
	*/

	conn.Connect(tarantoolURL)

	ticketCache = make(map[string]ticket)
	ontologyCache = make(map[string]Individual)
	mifCache = make(map[int]*ModuleInfoFile)
//	externalUsersTicketId = make(map[string]bool)

	//go monitorIndividualChanges()
	go func() {
		h := fasthttp.Server{
			Handler:            requestHandler,
			MaxRequestBodySize: 10 * 1024 * 1024 * 1024,

			// These timeouts trigger high iowait without the CL 34784
			// if many requests are sent over more than 100K
			// keep-alive http connections.

			ReadTimeout:  90 * time.Second,
			WriteTimeout: 5 * time.Second,
		}
		err = h.ListenAndServe("0.0.0.0:" + webserverPort)
		if err != nil {
			log.Fatal("ERR! ON STARTUP HTTP WEBSERVER ", err)
		}
	}()

	if useHTTPS {
		err = fasthttp.ListenAndServeTLS("0.0.0.0:"+webserverHTTPSPort, "ssl-certs/server.crt",
			"ssl-certs/server.key", requestHandler)
		if err != nil {
			log.Fatal("ERR! ON STARTUP HTTPS WEBSERVER", err)
		}
	}

	fmt.Println("web server ready, listen " + webserverPort)
	select {}
	/*
		err = fasthttp.ListenAndServeTLS("0.0.0.0:8020", "ssl-certs/server.crt",
			"ssl-certs/server.key", requestHandler)
		if err != nil {
			log.Fatal("ERR! ON STARTUP WEBSERVER ON HTTPS", err)
		}
	*/
}
