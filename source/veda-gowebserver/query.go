package main

import (
  "encoding/json"
  "github.com/itiu/fasthttp"
  "github.com/op/go-nanomsg"
  "log"
  "time"
  "strings"
)

//query function handle query request with fulltext search
//query request redirects to fr-query module via socket
func query(ctx *fasthttp.RequestCtx) {

  defer func() {
    if r := recover(); r != nil {
      log.Println("Recovered in query", r)
      ctx.Response.SetStatusCode(int(InternalServerError))
    }
  }()

  ctx.Response.Header.SetCanonical([]byte("Content-Type"), []byte("application/json"))
  timestamp := time.Now()

  var query string
  var sql string
  var ticketKey string
  var sort string
  var databases string
  var reopen bool
  var top int
  var from int
  var limit int

  ticketKey = string(ctx.QueryArgs().Peek("ticket")[:])
  if len(ticketKey) == 0 {
    ticketKey = string(ctx.Request.Header.Cookie("ticket"))
  }

  if ctx.IsGet() == true {
    query = string(ctx.QueryArgs().Peek("query")[:])
    sql = string(ctx.QueryArgs().Peek("sql")[:])
    sort = string(ctx.QueryArgs().Peek("sort")[:])
    databases = string(ctx.QueryArgs().Peek("databases")[:])
    reopen = ctx.QueryArgs().GetBool("reopen")
    _top, _ := ctx.QueryArgs().GetUint("top")
    _limit, _ := ctx.QueryArgs().GetUint("limit")
    _from, _ := ctx.QueryArgs().GetUint("from")

    top = int(_top)
    from = int(_from)
    limit = int(_limit)
  } else {
    var jsonData map[string]interface{}
    err := json.Unmarshal(ctx.Request.Body(), &jsonData)
    if err != nil {
      log.Println("ERR! get individuals: DECODING JSON REQUEST ", err)
      ctx.Response.SetStatusCode(int(InternalServerError))
      trail("", "", "query", make(map[string]interface{}), "{}", BadRequest, timestamp)
      return
    }

    if len(ticketKey) == 0 && jsonData["ticket"] != nil {
      ticketKey = jsonData["ticket"].(string)
    }

    if jsonData["query"] != nil {
      query = jsonData["query"].(string)
    }

    if jsonData["sql"] != nil {
      sql = jsonData["sql"].(string)
    }

    if jsonData["sort"] != nil {
      sort = jsonData["sort"].(string)
    }

    if jsonData["databases"] != nil {
      databases = jsonData["databases"].(string)
    }

    if jsonData["reopen"] != nil {
      reopen = jsonData["reopen"].(bool)
    }

    _top := jsonData["top"]
    if _top != nil {
      switch _top.(type) {
      case int64:
        top = int(_top.(int64))
      case uint64:
        top = int(_top.(uint64))
      case float64:
        top = int(_top.(float64))
      default:
        top = 0
      }
    }

    _limit := jsonData["limit"]
    if _limit != nil {
      switch _limit.(type) {
      case int64:
        limit = int(_limit.(int64))
      case uint64:
        limit = int(_limit.(uint64))
      case float64:
        limit = int(_limit.(float64))
      default:
        limit = 0
      }
    }

    _from := jsonData["from"]
    if _from != nil {
      switch _from.(type) {
      case int64:
        from = int(_from.(int64))
      case uint64:
        from = int(_from.(uint64))
      case float64:
        from = int(_from.(float64))
      default:
        from = 0
      }
    }
  }

  if top < 0 {
    top = 10000
  }

  if from < 0 {
    from = 0
  }

  if limit < 0 {
    limit = 10000
  }

  request := make([]interface{}, 8)
  //fills request map with parametrs

  var is_search_query = false

  if sql != "" || (strings.Contains(strings.ToLower(query), "select") && strings.Contains(strings.ToLower(query), "from")) {
        if use_clickhouse == "true" {
            is_search_query = true
        }
  }

  request[0] = ticketKey

  if is_search_query == true {
    if sql != "" {
        request[1] = sql
    } else {
        request[1] = query
    }
  } else {
    request[1] = query
  }

  request[2] = sort
  request[3] = databases
  request[4] = reopen
  request[5] = top
  request[6] = limit
  request[7] = from

  defer func() {
    if r := recover(); r != nil {
      log.Printf("ERR! fail execute(%s) request=%v\n", queryServiceURL, request)
      rc := InternalServerError
      ctx.Response.SetStatusCode(int(rc))
      trail1(ticketKey, "", "query", query, "", rc, timestamp)
      return
    }
  }()

  //Marshal request and send to socket
  jsonRequest, err := json.Marshal(request)
  if err != nil {
    log.Printf("ERR! QUERY: ENCODE JSON REQUEST: %s %v\n", request, err)
    ctx.Response.SetStatusCode(int(InternalServerError))
    return
  }

  if len(jsonRequest) == 0 {
    log.Printf("ERR! empty query: %s %v\n", request, err)
    ctx.Response.SetStatusCode(int(InternalServerError))
    return
  }

  var querySocket *nanomsg.Socket
  querySocket, err = nanomsg.NewSocket(nanomsg.AF_SP, nanomsg.REQ)
  if err != nil {
    log.Printf("ERR! ON CREATING QUERY SOCKET")
    ctx.Response.SetStatusCode(int(InternalServerError))
    return
  }

  err = querySocket.SetRecvMaxSize(1024*1024*10)
  if err != nil {
    log.Printf("ERR! FAIL SET MAX RECV SIZE, err=%v\n", err)
  }

  //log.Println("use query service url: ", queryServiceURL)
    if is_search_query {
        _, err = querySocket.Connect(searchQueryURL)
        if err != nil {
            log.Printf("ERR! ON CONNECT TO SEARCH QUERY SOCKET, url=%s, query=%s", searchQueryURL, string(jsonRequest))
            ctx.Response.SetStatusCode(int(InternalServerError))
            return
        }
    } else {
        _, err = querySocket.Connect(queryServiceURL)
        if err != nil {
            log.Printf("ERR! ON CONNECT TO FT QUERY SOCKET, url=%s, query=%s", queryServiceURL, string(jsonRequest))
            ctx.Response.SetStatusCode(int(InternalServerError))
            return
        }
    }


  cb, err1 := NmCSend(querySocket, jsonRequest, 0)
  if err1 != nil || cb <= 0 {
    log.Printf("ERR! ON SEND TO FT-QUERY SOCKET, url=%s, query=%s", queryServiceURL, string(jsonRequest))
    ctx.Response.SetStatusCode(int(InternalServerError))
    return
  }

  responseBuf, err := querySocket.Recv(0)

  querySocket.Close()

  ctx.Write(responseBuf)

  var jsonResponse map[string]interface{}
  err = json.Unmarshal(responseBuf, &jsonResponse)
  if err != nil {
    log.Printf("ERR! QUERY INDIVIDUAL: ENCODE JSON RESPONSE: %s %v\n", responseBuf, err)
    ctx.Response.SetStatusCode(int(InternalServerError))
    return
  }

  rc := jsonResponse["result_code"]

  var result_code = int(InternalServerError)
  if rc != nil {
    switch rc.(type) {
    case float64:
      result_code = int(rc.(float64))
    }
    ctx.Response.SetStatusCode(result_code)
  }

  trail1(ticketKey, "", "query", query, "", ResultCode(result_code), timestamp)
}
