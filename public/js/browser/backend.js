veda.Module(function Backend(veda) { "use strict";

	// Veda HTTP server functions

	$.ajaxSetup ({
		dataType: "json",
		cache: false
	});

	window.get_rights = function (ticket, uri, success, fail) {
		var params = {
			type: "GET",
			url: "get_rights",
			data: { "ticket": ticket, "uri": uri }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) { 
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.get_rights_origin = function (ticket, uri, success, fail) {
		var params = {
			type: "GET",
			url: "get_rights_origin",
			data: { "ticket": ticket, "uri": uri }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.authenticate = function (login, password, success, fail) {
		var params = {
			type: "GET",
			url: "authenticate",
			data: { "login": login, "password": password }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.is_ticket_valid = function (ticket, success, fail) {
		var params = {
			type: "GET",
			url: "is_ticket_valid",
			data: { "ticket": ticket }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return result.responseText;
		}
	}

	window.wait_pmodule = function (pmodule_id, success, fail) {
		var params = {
			type: "GET",
			url: "wait_pmodule",
			data: { "pmodule_id": pmodule_id }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.backup = function (callback) {
		var params = {
			type: "GET",
			url: "backup",
			data: { }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.count_individuals = function (callback) {
		var params = {
			type: "GET",
			url: "count_individuals",
			data: { }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) { 
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.set_trace = function (idx, state, success, fail) {
		var params = {
			type: "GET",
			url: "set_trace",
			data: { "idx": idx, "state" : state  }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.query = function (ticket, q, sort, databases, reopen, success, fail) {
		var params = {
			type: "GET",
			url: "query",
			data: { "ticket": ticket, "query": q, "sort": sort || null, "databases" : databases || null, "reopen" : reopen || false }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.get_individuals = function (ticket, uris, success, fail) {
		var params = {
			type: "POST",
			url: "get_individuals",
			data: JSON.stringify({ "ticket": ticket, "uris": uris }),
			contentType: "application/json"
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.get_count = 0, window.get_summary_time = 0;

	window.get_individual = function (ticket, uri, success, fail) {
		
		var t1, t2;
		t1 = Date.now();
		get_count++;
		
		var params = {
			type: "GET",
			url: "get_individual",
			data: { "ticket": ticket, "uri": uri }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			
			t2 = Date.now();
			get_summary_time += t2 - t1;
			
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText, function dateReviver (key, value) {
				return key === "data" && this.type === "Datetime" ? new Date(value) : value;
			});
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.put_individual = function (ticket, individual, wait_for_indexing, success, fail) {
		var params = {
			type: "PUT",
			url: "put_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.add_to_individual = function (ticket, individual, wait_for_indexing, success, fail) {
		var params = {
			type: "PUT",
			url: "add_to_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.set_in_individual = function (ticket, individual, wait_for_indexing, success, fail) {
		var params = {
			type: "PUT",
			url: "set_in_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.remove_from_individual = function (ticket, individual, wait_for_indexing, success, fail) {
		var params = {
			type: "PUT",
			url: "remove_from_individual",
			data: JSON.stringify({"ticket": ticket, "individual": individual, "wait_for_indexing" : wait_for_indexing || false }),
			contentType: "application/json"
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.get_property_value = function (ticket, uri, property_uri, success, fail) {
		var params = {
			type: "GET",
			url: "get_property_value",
			data: { "ticket": ticket, "uri": uri, "property_uri": property_uri }
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) {
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}

	window.execute_script = function (script, success, fail) {
		var params = {
			type: "POST",
			url: "execute_script",
			data: JSON.stringify({"script": script}),
			contentType: "application/json"
		};
		if(!success || !fail) {
			params.async = false;
			var result = $.ajax(params);
			if (result.status >= 400) { 
				veda.trigger("error", {status: result.status, description: result.statusText});
				throw {status: result.status, description: result.statusText};
			}
			return JSON.parse(result.responseText);
		}
		$.ajax(params).done(success).fail(fail);
	}
	
});
