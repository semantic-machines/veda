// HTTP server functions (sync)

if (typeof exports === 'object') {
  var XMLHttpRequest = require('./XMLHttpRequest.js').XMLHttpRequest;
  exports.Backend = Backend;
}

function Backend () {

//  var base = "http://127.0.0.1:8080/";
    var base = "";

  function call_server(params) {
    var method = params.method,
        url = base + params.url,
        data = params.data,
        xhr = new XMLHttpRequest();
    if (method === "GET") {
      var params = [];
      for (var name in data) {
        if (typeof data[name] !== "undefined") {
          params.push(name + "=" + encodeURIComponent(data[name]));
        }
      }
      params = params.join("&");
      xhr.open(method, url + "?" + params, false);
      xhr.send(null);
    } else {
      xhr.open(method, url, false);
      xhr.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
      var payload = JSON.stringify(data, function (key, value) {
        return key === "data" && (this.type === "Decimal") ? value.toString() : value;
      });
      xhr.send(payload);
    }
    if (xhr.status === 200) {
      return JSON.parse(
        xhr.responseText,
        function (key, value) {
          return key === "data" && this.type === "Datetime" ? new Date(value) :
                 key === "data" && (this.type === "Decimal") ? parseFloat(value) : value;
        }
      );
    } else {
      throw new Error(xhr.status);
    }
  }

  this.flush = function flush(module_id, wait_op_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "flush",
      data: {
        "module_id": isObj ? arg.module_id : module_id,
        "wait_op_id": isObj ? arg.wait_op_id : wait_op_id
      }
    };
    return call_server(params);
  }

  this.get_rights = function get_rights(ticket, uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_rights",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri
      }
    };
    return call_server(params);
  }

  this.get_rights_origin = function get_rights_origin(ticket, uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_rights_origin",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri
      }
    };
    return call_server(params);
  }

  this.get_membership = function get_membership(ticket, uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_membership",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri
      }
    };
    return call_server(params);
  }

  this.authenticate = function authenticate(login, password) {
    if (login == "VedaNTLMFilter")
        login = "cfg:Guest";
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "authenticate",
      data: {
        "login": isObj ? arg.login : login,
        "password": isObj ? arg.password : password
      }
    };
    return call_server(params);
  }

  this.get_ticket_trusted = function get_ticket_trusted(ticket, login) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_ticket_trusted",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "login": isObj ? arg.login : login
      }
    };
    return call_server(params);
  }

  this.is_ticket_valid = function is_ticket_valid(ticket) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "is_ticket_valid",
      data: {
        "ticket": isObj ? arg.ticket : ticket
      }
    };
    return call_server(params);
  }

  this.get_operation_state = function get_operation_state(module_id, wait_op_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_operation_state",
      data: {
        "module_id": isObj ? arg.module_id : module_id,
        "wait_op_id": isObj ? arg.wait_op_id : wait_op_id
      }
    };
    return call_server(params);
  }

  this.wait_module = function wait_module(module_id, in_op_id) {
    var timeout = 1;
    var op_id_from_module;
    for (var i = 0; i < 100; i++) {
      op_id_from_module = this.get_operation_state (module_id, in_op_id);
      if (op_id_from_module >= in_op_id) { break; }
      var endtime = new Date().getTime() + timeout;
      while (new Date().getTime() < endtime);
      timeout += 2;
    }
  }

  this.restart = function restart(ticket) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "restart",
      data: {
        "ticket": isObj ? arg.ticket : ticket
      }
    };
    return call_server(params);
  }

  this.backup = function backup(to_binlog) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "backup",
      data: {
        "to_binlog": isObj ? arg.to_binlog : to_binlog
      }
    };
    return call_server(params);
  }

  this.count_individuals = function count_individuals() {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "count_individuals",
      data: {}
    };
    return call_server(params);
  }

  this.set_trace = function set_trace(idx, state) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "set_trace",
      data: {
        "idx": isObj ? arg.idx : idx,
        "state" : isObj ? arg.state : state
      }
    };
    return call_server(params);
  }

  this.query = function query(ticket, query, sort, databases, reopen, top, limit, from) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "query",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "query": isObj ? arg.query : query,
        "sort": isObj ? arg.sort : sort,
        "databases" : isObj ? arg.databases : databases,
        "reopen" : isObj ? arg.reopen : reopen,
        "top" : isObj ? arg.top : top,
        "limit" : isObj ? arg.limit : limit,
        "from"  : isObj ? arg.from : from
      }
    };
    return call_server(params);
  }

  this.get_individual = function get_individual(ticket, uri, reopen) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_individual",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri,
        "reopen" : (isObj ? arg.reopen : reopen) || false
      }
    };
    return call_server(params);
  }

  this.get_individuals = function get_individuals(ticket, uris) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "POST",
      url: "get_individuals",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uris": isObj ? arg.uris : uris
      }
    };
    return call_server(params);
  }

  //////////////////////////

  this.remove_individual = function remove_individual(ticket, uri, assigned_subsystems, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "PUT",
      url: "remove_individual",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri,
        "assigned_subsystems": (isObj ? arg.assigned_subsystems : assigned_subsystems) || 0,
        "prepare_events": true,
        "event_id": (isObj ? arg.event_id : event_id) || "",
        "transaction_id": (isObj ? arg.transaction_id : transaction_id) || ""
      }
    };
    return call_server(params);
  }

  this.put_individual = function put_individual(ticket, individual, assigned_subsystems, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "PUT",
      url: "put_individual",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "assigned_subsystems" : (isObj ? arg.assigned_subsystems : assigned_subsystems) || 0,
        "prepare_events": true,
        "event_id" : (isObj ? arg.event_id : event_id) || "",
        "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
      }
    };
    return call_server(params);
  }

  this.add_to_individual = function add_to_individual(ticket, individual, assigned_subsystems, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "PUT",
      url: "add_to_individual",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "assigned_subsystems": (isObj ? arg.assigned_subsystems : assigned_subsystems) || 0,
        "prepare_events": true,
        "event_id": (isObj ? arg.event_id : event_id) || "",
        "transaction_id": (isObj ? arg.transaction_id : transaction_id) || ""
      }
    };
    return call_server(params);
  }

  this.set_in_individual = function set_in_individual(ticket, individual, assigned_subsystems, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "PUT",
      url: "set_in_individual",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "assigned_subsystems" : (isObj ? arg.assigned_subsystems : assigned_subsystems) || 0,
        "prepare_events": true,
        "event_id" : (isObj ? arg.event_id : event_id) || "",
        "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
      }
    };
    return call_server(params);
  }

  this.remove_from_individual = function remove_from_individual(ticket, individual, assigned_subsystems, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "PUT",
      url: "remove_from_individual",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "individual": isObj ? arg.individual : individual,
        "assigned_subsystems" : (isObj ? arg.assigned_subsystems : assigned_subsystems) || 0,
        "prepare_events": true,
        "event_id" : (isObj ? arg.event_id : event_id) || "",
        "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
      }
    };
    return call_server(params);
  }

  this.put_individuals = function put_individuals(ticket, individuals, assigned_subsystems, event_id, transaction_id) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "PUT",
      url: "put_individuals",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "individuals": isObj ? arg.individuals : individuals,
        "assigned_subsystems" : (isObj ? arg.assigned_subsystems : assigned_subsystems) || 0,
        "prepare_events": true,
        "event_id" : (isObj ? arg.event_id : event_id) || "",
        "transaction_id" : (isObj ? arg.transaction_id : transaction_id) || ""
      }
    };
    return call_server(params);
  }

  /////////////////////////////////////////

  this.get_property_value = function get_property_value(ticket, uri, property_uri) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "GET",
      url: "get_property_value",
      data: {
        "ticket": isObj ? arg.ticket : ticket,
        "uri": isObj ? arg.uri : uri,
        "property_uri": isObj ? arg.property_uri : property_uri
      }
    };
    return call_server(params);
  }

  this.execute_script = function execute_script(script) {
    var arg = arguments[0];
    var isObj = typeof arg === "object";
    var params = {
      method: "POST",
      url: "execute_script",
      data: {
        "script": isObj ? arg.script : script
      }
    };
    return call_server(params);
  }

}
