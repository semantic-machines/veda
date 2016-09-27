veda.Module(function Backend(veda) { "use strict";

	// Veda HTTP server functions

	$.ajaxSetup ({
		dataType: "json",
		cache: false,
		timeout: 3000
	});

	function call_server(ticket, params, success, fail) {
		if( !(success && fail) ) {
			params.async = false;
			var res = $.ajax(params);
			if (res.status >= 400 || res.status == 0) { 
				veda.trigger("error", {status: res.status, description: res.statusText});
				throw {status: res.status, description: res.statusText};
			}
			var result;
			try { 
				result = JSON.parse(res.responseText);
			} catch (e) {
				result = res.responseText;
			} finally {
				return result;
			}
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.get_rights = function (ticket, uri, success, fail) {
		var params = {
			type: "GET",
			url: "get_rights",
			data: { "ticket": ticket, "uri": uri }
		};
		return call_server(ticket, params, success, fail);
	}

	window.get_rights_origin = function (ticket, uri, success, fail) {
		var params = {
			type: "GET",
			url: "get_rights_origin",
			data: { "ticket": ticket, "uri": uri }
		};
		return call_server(ticket, params, success, fail);
	}

	window.authenticate = function (login, password, success, fail) {
		var params = {
			type: "GET",
			url: "authenticate",
			data: { "login": login, "password": password }
		};
		return call_server(undefined, params, success, fail);
	}

	window.get_ticket_trusted = function (ticket, login, success, fail) {
		var params = {
			type: "GET",
			url: "get_ticket_trusted",
			data: { "ticket": ticket, "login": login }
		};
		return call_server(undefined, params, success, fail);
	}

	window.is_ticket_valid = function (ticket, success, fail) {
		var params = {
			type: "GET",
			url: "is_ticket_valid",
			data: { "ticket": ticket }
		};
		return call_server(undefined, params, success, fail);
	}

	window.get_operation_state = function (module_id, wait_op_id, success, fail) {
		var params = {
			type: "GET",
			url: "get_operation_state",
			data: { "module_id": module_id, "wait_op_id": wait_op_id }
		};
		return call_server(undefined, params, success, fail);
	}

        window.wait_module = function (module_id, op_id, success, fail)
        {
                var timeout = 1;
                var op_id_from_module;
                for (var i = 0; i < 100; i++)
                {
                        op_id_from_module = get_operation_state (module_id, op_id);

                        if (op_id_from_module >= op_id)
                                break;

                        var endtime = new Date().getTime() + timeout;
                        while (new Date().getTime() < endtime);

                        timeout += 1;
                }
        }

	window.restart = function (ticket, success, fail) {
		var params = {
			type: "GET",
			url: "restart",
			data: { "ticket": ticket }
		};
		return call_server(undefined, params, success, fail);
	}
	
	window.backup = function (to_binlog, success, fail) {
		var params = {
			type: "GET",
			url: "backup",
			data: { "to_binlog": to_binlog }
		};
		return call_server(undefined, params, success, fail);
	}

	window.count_individuals = function (success, fail) {
		var params = {
			type: "GET",
			url: "count_individuals",
			data: { }
		};
		return call_server(undefined, params, success, fail);
	}

	window.set_trace = function (idx, state, success, fail) {
		var params = {
			type: "GET",
			url: "set_trace",
			data: { "idx": idx, "state" : state  }
		};
		return call_server(undefined, params, success, fail);
	}

	window.query = function (ticket, q, sort, databases, reopen, top, limit, success, fail) {
		var params = {
			type: "GET",
			url: "query",
			data: { "ticket": ticket, "query": q, "sort": sort || null, "databases" : databases || null, "reopen" : reopen || false, 
					"top" : top || 0, "limit" : limit || 0}
		};
		return call_server(ticket, params, success, fail);
	}

	window.get_individuals = function (ticket, uris, success, fail) {
		var params = {
			type: "POST",
			url: "get_individuals",
			data: JSON.stringify({ "ticket": ticket, "uris": uris }),
			contentType: "application/json"
		};
		return call_server(ticket, params, success, fail);
	}

	window.get_count = 0, window.get_summary_time = 0;

	window.get_individual = function (ticket, uri, reopen, success, fail) {
		
		var t1, t2;
		t1 = Date.now();
		get_count++;
		
		var params = {
			type: "GET",
			url: "get_individual",
			data: { "ticket": ticket, "uri": uri, "reopen" : reopen || false}
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			
			t2 = Date.now();
			get_summary_time += t2 - t1;
			
			if (result.status >= 400 || result.status == 0) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText, function dateReviver (key, value) {
				return key === "data" && this.type === "Datetime" ? new Date(value) : value;
			});
		}
		$.ajax(params).done(success).fail(fail);
	}

//////////////////////////

	window.remove_individual = function (ticket, uri, prepare_events, event_id, transaction_id, success, fail) {
		var params = {
			type: "PUT",
			url: "remove_individual",
			data: JSON.stringify({"ticket": ticket, "uri": uri, 
			"prepare_events" : prepare_events || true,  "event_id" : event_id || "", "transaction_id" : transaction_id || ""}),
			contentType: "application/json"
		};
		return call_server(ticket, params, success, fail);
	}

	window.put_individual = function (ticket, individual, prepare_events, event_id, transaction_id, success, fail) {
		var params = {
			type: "PUT",
			url: "put_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, 
			"prepare_events" : prepare_events || true,  "event_id" : event_id || "", "transaction_id" : transaction_id || ""}),
			contentType: "application/json"
		};
		return call_server(ticket, params, success, fail);
	}

	window.add_to_individual = function (ticket, individual, prepare_events, event_id, transaction_id, success, fail) {
		var params = {
			type: "PUT",
			url: "add_to_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, 
			"prepare_events" : prepare_events || true,  "event_id" : event_id || "", "transaction_id" : transaction_id || ""}),
			contentType: "application/json"
		};
		return call_server(ticket, params, success, fail);
	}

	window.set_in_individual = function (ticket, individual, prepare_events, event_id, transaction_id, success, fail) {
		var params = {
			type: "PUT",
			url: "set_in_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual,
			"prepare_events" : prepare_events || true,  "event_id" : event_id || "", "transaction_id" : transaction_id || ""}),
			contentType: "application/json"
		};
		return call_server(ticket, params, success, fail);
	}

	window.remove_from_individual = function (ticket, individual, prepare_events, event_id, transaction_id, success, fail) {
		var params = {
			type: "PUT",
			url: "remove_from_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual,
			"prepare_events" : prepare_events || true,  "event_id" : event_id || "", "transaction_id" : transaction_id || ""}),
			contentType: "application/json"
		};
		return call_server(ticket, params, success, fail);
	}

/////////////////////////////////////////

	window.get_property_value = function (ticket, uri, property_uri, success, fail) {
		var params = {
			type: "GET",
			url: "get_property_value",
			data: { "ticket": ticket, "uri": uri, "property_uri": property_uri }
		};
		return call_server(ticket, params, success, fail);
	}

	window.execute_script = function (script, success, fail) {
		var params = {
			type: "POST",
			url: "execute_script",
			data: JSON.stringify({"script": script}),
			contentType: "application/json"
		};
		return call_server(undefined, params, success, fail);
	}
	
});
