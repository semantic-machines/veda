veda.Module(function Backend(veda) { "use strict";

  // Veda HTTP server functions

  $.ajaxSetup ({
    dataType: "json",
    cache: false,
    timeout: 3000
  });

  function call_server(ticket, params, async) {
    if( !async ) {
      params.async = false;
      var res = $.ajax(params);
      if (res.status >= 400 || res.status == 0) {
        veda.trigger("danger", {status: res.status, description: res.statusText});
        throw {status: res.status, description: res.statusText};
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
      } catch (e) {
        result = res.responseText;
      } finally {
        return result;
      }
    } else {
      return $.ajax(params);
    }
  }

  window.flush = function (module_id, wait_op_id, async) {
    var params = {
      type: "GET",
      url: "flush",
      data: {
        "module_id": module_id,
        "wait_op_id": wait_op_id
      }
    };
    return call_server(undefined, params, async);
  }

  window.get_rights = function (ticket, uri, async) {
    var params = {
      type: "GET",
      url: "get_rights",
      data: {
        "ticket": ticket,
        "uri": uri
      }
    };
    return call_server(ticket, params, async);
  }

  window.get_rights_origin = function (ticket, uri, async) {
    var params = {
      type: "GET",
      url: "get_rights_origin",
      data: {
        "ticket": ticket,
        "uri": uri
      }
    };
    return call_server(ticket, params, async);
  }

  window.get_membership = function (ticket, uri, async) {
    var params = {
      type: "GET",
      url: "get_membership",
      data: {
        "ticket": ticket,
        "uri": uri
      }
    };
    return call_server(ticket, params, async);
  }

  window.authenticate = function (login, password, async) {
    var params = {
      type: "GET",
      url: "authenticate",
      data: {
        "login": login,
        "password": password
      }
    };
    return call_server(undefined, params, async);
  }

  window.get_ticket_trusted = function (ticket, login, async) {
    var params = {
      type: "GET",
      url: "get_ticket_trusted",
      data: {
        "ticket": ticket,
        "login": login
      }
    };
    return call_server(undefined, params, async);
  }

  window.is_ticket_valid = function (ticket, async) {
    var params = {
      type: "GET",
      url: "is_ticket_valid",
      data: {
        "ticket": ticket
      }
    };
    return call_server(undefined, params, async);
  }

  window.get_operation_state = function (module_id, wait_op_id, async) {
    var params = {
      type: "GET",
      url: "get_operation_state",
      data: {
        "module_id": module_id,
        "wait_op_id": wait_op_id
      }
    };
    return call_server(undefined, params, async);
  }

  window.wait_module = function (module_id, op_id, async) {
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

  window.restart = function (ticket, async) {
    var params = {
      type: "GET",
      url: "restart",
      data: {
        "ticket": ticket
      }
    };
    return call_server(undefined, params, async);
  }

  window.backup = function (to_binlog, async) {
    var params = {
      type: "GET",
      url: "backup",
      data: {
        "to_binlog": to_binlog
      }
    };
    return call_server(undefined, params, async);
  }

  window.count_individuals = function (async) {
    var params = {
      type: "GET",
      url: "count_individuals",
      data: {}
    };
    return call_server(undefined, params, async);
  }

  window.set_trace = function (idx, state, async) {
    var params = {
      type: "GET",
      url: "set_trace",
      data: {
        "idx": idx,
        "state" : state
      }
    };
    return call_server(undefined, params, async);
  }

  window.query = function (ticket, q, sort, databases, reopen, top, limit, async, from) {
    var params = {
      type: "GET",
      url: "query",
      data: {
        "ticket": ticket,
        "query": q,
        "sort": sort || null,
        "databases" : databases || null,
        "reopen" : reopen || false,
	"from"	: from || 0,
        "top" : top || 0,
        "limit" : limit || 1000
      }
    };
    return call_server(ticket, params, async);
  }

  window.get_individual = function (ticket, uri, reopen, async) {
    var params = {
      type: "GET",
      url: "get_individual",
      data: {
        "ticket": ticket,
        "uri": uri,
        "reopen" : reopen || false
      }
    };
    return call_server(ticket, params, async);
  }

  window.get_individuals = function (ticket, uris, async) {
    var params = {
      type: "POST",
      url: "get_individuals",
      data: JSON.stringify({
        "ticket": ticket,
        "uris": uris
      }),
      contentType: "application/json"
    };
    return call_server(ticket, params, async);
  }

//////////////////////////

  window.remove_individual = function (ticket, uri, prepare_events, event_id, transaction_id, async) {
    var params = {
      type: "PUT",
      url: "remove_individual",
      data: JSON.stringify({
        "ticket": ticket,
        "uri": uri,
        "prepare_events": prepare_events || true,
        "event_id": event_id || "",
        "transaction_id": transaction_id || ""
      }),
      contentType: "application/json"
    };
    return call_server(ticket, params, async);
  }

  window.put_individual = function (ticket, individual, prepare_events, event_id, transaction_id, async) {
    var params = {
      type: "PUT",
      url: "put_individual",
      data: JSON.stringify(
        {
          "ticket": ticket,
          "individual": individual,
          "prepare_events" : prepare_events || true,
          "event_id" : event_id || "",
          "transaction_id" : transaction_id || ""
        },
        function (key, value) {
          return key === "data" && (this.type === "Decimal" || this.type === _Decimal) ? value.toString() : value;
        }
      ),
      contentType: "application/json"
    };
    return call_server(ticket, params, async);
  }

  window.add_to_individual = function (ticket, individual, prepare_events, event_id, transaction_id, async) {
    var params = {
      type: "PUT",
      url: "add_to_individual",
      data: JSON.stringify({
        "ticket": ticket,
        "individual": individual,
        "prepare_events": prepare_events || true,
        "event_id": event_id || "",
        "transaction_id": transaction_id || ""
      }),
      contentType: "application/json"
    };
    return call_server(ticket, params, async);
  }

  window.set_in_individual = function (ticket, individual, prepare_events, event_id, transaction_id, async) {
    var params = {
      type: "PUT",
      url: "set_in_individual",
      data: JSON.stringify({
        "ticket": ticket,
        "individual": individual,
        "prepare_events" : prepare_events || true,
        "event_id" : event_id || "",
        "transaction_id" : transaction_id || ""
      }),
      contentType: "application/json"
    };
    return call_server(ticket, params, async);
  }

  window.remove_from_individual = function (ticket, individual, prepare_events, event_id, transaction_id, async) {
    var params = {
      type: "PUT",
      url: "remove_from_individual",
      data: JSON.stringify({
        "ticket": ticket,
        "individual": individual,
        "prepare_events" : prepare_events || true,
        "event_id" : event_id || "",
        "transaction_id" : transaction_id || ""
      }),
      contentType: "application/json"
    };
    return call_server(ticket, params, async);
  }

/////////////////////////////////////////

  window.get_property_value = function (ticket, uri, property_uri, async) {
    var params = {
      type: "GET",
      url: "get_property_value",
      data: {
        "ticket": ticket,
        "uri": uri,
        "property_uri": property_uri
      }
    };
    return call_server(ticket, params, async);
  }

  window.execute_script = function (script, async) {
    var params = {
      type: "POST",
      url: "execute_script",
      data: JSON.stringify({
        "script": script
      }),
      contentType: "application/json"
    };
    return call_server(undefined, params, async);
  }

});
