veda.Module(function Backend(veda) { "use strict";

	// Veda HTTP server functions

	$.ajaxSetup ({
		dataType: "json",
		cache: false
	});

	window.get_rights = function (ticket, uri, callback) {
		var params = {
			type: "GET",
			url: "get_rights",
			data: { "ticket": ticket, "uri": uri }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.get_rights_origin = function (ticket, uri, callback) {
		var params = {
			type: "GET",
			url: "get_rights_origin",
			data: { "ticket": ticket, "uri": uri }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.authenticate = function (login, password, callback) {
		var params = {
				type: "GET",
				url: "authenticate",
				data: { "login": login, "password": password }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.is_ticket_valid = function (ticket, callback) {
		var params = {
			type: "GET",
			url: "is_ticket_valid",
			data: { "ticket": ticket }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return result.responseText;
		}
	}

	window.wait_pmodule = function (pmodule_id, callback) {
		var params = {
			type: "GET",
			url: "wait_pmodule",
			data: { "pmodule_id": pmodule_id }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.backup = function (callback) {
		var params = {
			type: "GET",
			url: "backup",
			data: { }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.count_individuals = function (callback) {
		var params = {
			type: "GET",
			url: "count_individuals",
			data: { }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.set_trace = function (idx, state, callback) {
		var params = {
			type: "GET",
			url: "set_trace",
			data: { "idx": idx, "state" : state  }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.query = function (ticket, q, sort, databases, reopen, callback) {
		var params = {
			type: "GET",
			url: "query",
			data: { "ticket": ticket, "query": q, "sort": sort || null, "databases" : databases || null, "reopen" : reopen || false }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.get_individuals = function (ticket, uris, callback) {
		var params = {
			type: "POST",
			url: "get_individuals",
			data: JSON.stringify({ "ticket": ticket, "uris": uris }),
			contentType: "application/json"
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.get_count = 0, window.get_summary_time = 0;

	window.get_individual = function (ticket, uri, callback) {
		
		var t1, t2;
		t1 = Date.now();
		get_count++;
		
		var params = {
			type: "GET",
			url: "get_individual",
			data: { "ticket": ticket, "uri": uri }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			
			t2 = Date.now();
			get_summary_time += t2 - t1;
			
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText, function dateReviver (key, value) {
				return key === "data" && this.type === "Datetime" ? new Date(value) : value;
			});
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.put_individual = function (ticket, individual, wait_for_indexing, callback) {
		var params = {
			type: "PUT",
			url: "put_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.add_to_individual = function (ticket, individual, wait_for_indexing, callback) {
		var params = {
			type: "PUT",
			url: "add_to_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.set_in_individual = function (ticket, individual, wait_for_indexing, callback) {
		var params = {
			type: "PUT",
			url: "set_in_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.remove_from_individual = function (ticket, individual, wait_for_indexing, callback) {
		var params = {
			type: "PUT",
			url: "remove_from_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.get_property_value = function (ticket, uri, property_uri, callback) {
		var params = {
			type: "GET",
			url: "get_property_value",
			data: { "ticket": ticket, "uri": uri, "property_uri": property_uri }
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}

	window.execute_script = function (script, callback) {
		var params = {
			type: "POST",
			url: "execute_script",
			data: JSON.stringify({"script": script}),
			contentType: "application/json"
		};
		if(!callback) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) throw {status: result.status, description: result.statusText};
			return JSON.parse(result.responseText);
		}
		$.ajax(params)
			.fail( function () { throw {status: result.status, description: result.statusText}; } )
			.done( function (data) { callback(data); } );
	}
	
});
