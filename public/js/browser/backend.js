// Veda HTTP server functions
veda.Module(function Backend(veda) { "use strict";

  $.ajaxSetup ({
    dataType: "json",
    cache: false,
    timeout: 30000,
    async: false
  });

  // Check server health
  var notify = veda.Notify ? new veda.Notify() : function () {};
  var interval;
  function serverWatch() {
    if (interval) { return; }
    var duration = 10000;
    notify("danger", {name: "Connection error"});
    interval = setInterval(function () {
      try {
        var ontoVsn = get_individual(veda.ticket, "cfg:OntoVsn");
        if (ontoVsn) {
          clearInterval(interval);
          interval = undefined;
          notify("success", {name: "Connection restored"});
        } else {
          notify("danger", {name: "Connection error"});
        }
      } catch (ex) {
        notify("danger", {name: "Connection error"});
      }
    }, duration);
  }

  // Server errors
  function BackendError (result) {
    var errorCodes = {
         0: "Server unavailable",
       200: "Ok",
       201: "Created",
       204: "No content",
       400: "Bad request",
       403: "Forbidden",
       404: "Not found",
       422: "Unprocessable entity",
       429: "Too many requests",
       470: "Ticket not found",
       471: "Ticket expired",
       472: "Not authorized",
       473: "Authentication failed",
       474: "Not ready",
       475: "Fail open transaction",
       476: "Fail commit",
       477: "Fail store",
       500: "Internal server error",
       501: "Not implemented",
       503: "Service unavailable",
       904: "Invalid identifier",
      1021: "Disk full",
      1022: "Duplicate key",
      1118: "Size too large",
      4000: "Connect error"
    };
    this.code = result.status;
    this.name = errorCodes[this.code];
    this.status = result.status;
    this.message = errorCodes[this.code];
    this.stack = (new Error()).stack;
    if (result.status === 0) {
      serverWatch();
    }
    if (result.status === 470 || result.status === 471) {
      veda.logout();
    }
  }
  BackendError.prototype = Object.create(Error.prototype);
  BackendError.prototype.constructor = BackendError;

  // Common server call function
  function call_server(params) {
    if( !params.async ) {
      var res = $.ajax(params);
      if (res.status >= 400) {
        throw new BackendError(res);
      }
      var result;
      try {
        // Parse with date & decimal reviver
        result = JSON.parse(
          res.responseText,
          function (key, value) {
            return key === "data" && this.type === "Datetime" ? new Date(value) :
                   key === "data" && (this.type === "Decimal" || this.type === _Decimal) ? parseFloat(value) : value;
          }
        );
      } catch (err) {
        result = res.responseText;
      } finally {
        return result;
      }
    } else {
      return $.ajax(params).catch(function (err) {
        throw new BackendError(err);
      });
    }
  }

  // Deferred (promise) only version
  /*function call_server(params) {
    return $.ajax(params);
  }*/

  window.flush = function (module_id, wait_op_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "flush",
      async: isObj ? arg.async : false,
      data: {
        "module_id": isObj ? arg.module_id : module_id,
        "wait_op_id": isObj ? arg.wait_op_id : wait_op_id
      }
    };
    return call_server(params);
  }

  window.get_rights = function (ticket, uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_rights",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri
      }
    };
    return call_server(params);
  }

  window.get_rights_origin = function (ticket, uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_rights_origin",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri
      }
    };
    return call_server(params);
  }

  window.get_membership = function (ticket, uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_membership",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri
      }
    };
    return call_server(params);
  }

  window.authenticate = function (login, password) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "authenticate",
      async: isObj ? arg.async : false,
      data: {
        "login": isObj ? arg.login : login,
        "password": isObj ? arg.password : password
      }
    };
    return call_server(params);
  }

  window.get_ticket_trusted = function (ticket, login) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_ticket_trusted",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "login": isObj ? arg.login : login
      }
    };
    return call_server(params);
  }

  window.is_ticket_valid = function (ticket) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "is_ticket_valid",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket
      }
    };
    return call_server(params);
  }

  window.get_operation_state = function (module_id, wait_op_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_operation_state",
      async: isObj ? arg.async : false,
      data: {
        "module_id": isObj ? arg.module_id : module_id,
        "wait_op_id": isObj ? arg.wait_op_id : wait_op_id
      }
    };
    return call_server(params);
  }

  window.wait_module = function (module_id, op_id) {
    var timeout = 1;
    var op_id_from_module;
    for (var i = 0; i < 100; i++) {
      op_id_from_module = get_operation_state (module_id, op_id);
      if (op_id_from_module >= op_id) { break; }
      var endtime = new Date().getTime() + timeout;
      while (new Date().getTime() < endtime);
      timeout += 1;
    }
  }

  window.restart = function (ticket) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "restart",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket
      }
    };
    return call_server(params);
  }

  window.backup = function (to_binlog) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "backup",
      async: isObj ? arg.async : false,
      data: {
        "to_binlog": isObj ? arg.to_binlog : to_binlog
      }
    };
    return call_server(params);
  }

  window.count_individuals = function () {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "count_individuals",
      async: isObj ? arg.async : false,
      data: {}
    };
    return call_server(params);
  }

  window.set_trace = function (idx, state) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "set_trace",
      async: isObj ? arg.async : false,
      data: {
        "idx": isObj ? arg.idx : idx,
        "state" : isObj ? arg.state : state
      }
    };
    return call_server(params);
  }

  window.query = function (ticket, query, sort, databases, reopen, top, limit, from) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "query",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "query": isObj ? arg.query : query,
        "sort": (isObj ? arg.sort : sort) || null,
        "databases" : (isObj ? arg.databases : databases) || null,
        "reopen" : (isObj ? arg.reopen : reopen) || false,
        "top" : (isObj ? arg.top : top) || 0,
        "limit" : (isObj ? arg.limit : limit) || 1000,
        "from"  : (isObj ? arg.from : from) || 0
      }
    };
    return call_server(params);
  }

  window.get_individual = function (ticket, uri, reopen) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_individual",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri,
        "reopen" : (isObj ? arg.reopen : reopen) || false
      }
    };
    return call_server(params);
  }

  window.get_individuals = function (ticket, uris) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "POST",
      url: "get_individuals",
      async: isObj ? arg.async : false,
      data: JSON.stringify({
        "ticket": isObj ? arg.ticket : ticket,
        "uris": isObj ? arg.uris : uris
      }),
      contentType: "application/json"
    };
    return call_server(params);
  }

//////////////////////////

  window.remove_individual = function (ticket, uri, prepare_events, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "PUT",
      url: "remove_individual",
      async: isObj ? arg.async : false,
      data: JSON.stringify({
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri,
        "prepare_events": (isObj ? arg.prepare_events : prepare_events) || true,
        "event_id": (isObj ? arg.event_id : event_id) || "",
        "transaction_id": (isObj ? arg.transaction_id : transaction_id) || ""
      }),
      contentType: "application/json"
    };
    return call_server(params);
  }

  window.put_individual = function (ticket, individual, prepare_events, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "PUT",
      url: "put_individual",
      async: isObj ? arg.async : false,
      data: JSON.stringify(
        {
          "ticket": isObj ? arg.ticket : ticket,
          "individual": isObj ? arg.individual : individual,
          "prepare_events" : (isObj ? arg.prepare_events : prepare_events) || true,
          "event_id" : (isObj ? arg.event_id : event_id) || "",
          "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
        },
        function (key, value) {
          return key === "data" && (this.type === "Decimal" || this.type === _Decimal) ? value.toString() : value;
        }
      ),
      contentType: "application/json"
    };
    return call_server(params);
  }

  window.add_to_individual = function (ticket, individual, prepare_events, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "PUT",
      url: "add_to_individual",
      async: isObj ? arg.async : false,
      data: JSON.stringify({
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "prepare_events": (isObj ? arg.prepare_events : prepare_events) || true,
        "event_id": (isObj ? arg.event_id : event_id) || "",
        "transaction_id": (isObj ? arg.transaction_id : transaction_id) || ""
      }),
      contentType: "application/json"
    };
    return call_server(params);
  }

  window.set_in_individual = function (ticket, individual, prepare_events, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "PUT",
      url: "set_in_individual",
      async: isObj ? arg.async : false,
      data: JSON.stringify({
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "prepare_events" : (isObj ? arg.prepare_events : prepare_events) || true,
        "event_id" : (isObj ? arg.event_id : event_id) || "",
        "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
      }),
      contentType: "application/json"
    };
    return call_server(params);
  }

  window.remove_from_individual = function (ticket, individual, prepare_events, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "PUT",
      url: "remove_from_individual",
      async: isObj ? arg.async : false,
      data: JSON.stringify({
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "prepare_events" : (isObj ? arg.prepare_events : prepare_events) || true,
        "event_id" : (isObj ? arg.event_id : event_id) || "",
        "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
      }),
      contentType: "application/json"
    };
    return call_server(params);
  }

  window.put_individuals = function (ticket, individuals, prepare_events, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "PUT",
      url: "put_individuals",
      async: isObj ? arg.async : false,
      data: JSON.stringify(
        {
          "ticket": isObj ? arg.ticket : ticket,
          "individuals": isObj ? arg.individuals : individuals,
          "prepare_events" : (isObj ? arg.prepare_events : prepare_events) || true,
          "event_id" : (isObj ? arg.event_id : event_id) || "",
          "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
        },
        function (key, value) {
          return key === "data" && (this.type === "Decimal" || this.type === _Decimal) ? value.toString() : value;
        }
      ),
      contentType: "application/json"
    };
    return call_server(params);
  }

/////////////////////////////////////////

  window.get_property_value = function (ticket, uri, property_uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "GET",
      url: "get_property_value",
      async: isObj ? arg.async : false,
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri,
        "property_uri": isObj ? arg.property_uri : property_uri
      }
    };
    return call_server(params);
  }

  window.execute_script = function (script) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      type: "POST",
      url: "execute_script",
      async: isObj ? arg.async : false,
      data: JSON.stringify({
        "script": isObj ? arg.script : script
      }),
      contentType: "application/json"
    };
    return call_server(params);
  }

});
