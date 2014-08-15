// Veda HTTP server functions

$.ajaxSetup ({
	dataType: "json",
	cache: false
});

function get_rights(ticket, uri, callback) {
	var params = {
		type: "GET",
		url: "get_rights",
		data: { "ticket": ticket, "uri": uri }
	};
	if(!callback) {
		params.async = false;
		return JSON.parse($.ajax(params).responseText);
	}
	$.ajax(params)
		.done( function (data) { done(data) } )
		.fail( function (data, code) { fail(data, code) } );
}

function get_rights_origin(ticket, uri, callback) {
	var params = {
		type: "GET",
		url: "get_rights_origin",
		data: { "ticket": ticket, "uri": uri }
	};
	if(!callback) {
		params.async = false;
		return JSON.parse($.ajax(params).responseText);
	}
	$.ajax(params)
		.done( function (data) { done(data) } )
		.fail( function (data, code) { fail(data, code) } );
}

function authenticate(login, password, done, fail) {
	var params = {
			type: "GET",
			url: "authenticate",
			data: { "login": login, "password": password }
	};
	if(!done) {
		params.async = false;
		return JSON.parse($.ajax(params).responseText);
	}
	$.ajax(params)
		.done( function (data) { done(data) } )
		.fail( function (data, code) { fail(data, code) } );
}

function is_ticket_valid(ticket, done, fail) {
	var params = {
		type: "GET",
		url: "is_ticket_valid",
		data: { "ticket": ticket }
	};
	if(!done) {
		params.async = false;
		return $.ajax(params).responseText; //JSON.parse($.ajax(params).responseText);
	}
	$.ajax(params)
		.done( function (data) { done(data) } )
		.fail( function (data, code) { fail(data, code) } );
}

function wait_pmodule(pmodule_id, callback) {
	var params = {
		type: "GET",
		url: "wait_pmodule",
		data: { "pmodule_id": pmodule_id }
	};
	if(!callback) {
		params.async = false;
		return JSON.parse($.ajax(params).responseText);
	}
	$.ajax(params)
		.fail( function () { callback(false) } )
		.done( function (data) { callback(data) } );
}

function backup(callback) {
	var params = {
		type: "GET",
		url: "backup",
		data: { }
	};
	if(!callback) {
		params.async = false;
		return JSON.parse($.ajax(params).responseText);
	}
	$.ajax(params)
		.fail( function () { callback(false) } )
		.done( function (data) { callback(data) } );
}

function count_individuals(callback) {
	var params = {
		type: "GET",
		url: "count_individuals",
		data: { }
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function set_trace(idx, state, callback) {
	var params = {
		type: "GET",
		url: "set_trace",
		data: { "idx": idx, "state" : state  }
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function query(ticket, query, callback) {
	var params = {
		type: "GET",
		url: "query",
		data: { "ticket": ticket, "query": query }
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function get_individuals(ticket, uris, callback) {
	var params = {
		type: "POST",
		url: "get_individuals",
		data: JSON.stringify({ "ticket": ticket, "uris": uris }),
		contentType: "application/json"
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function get_individual(ticket, uri, callback) {
	var params = {
		type: "GET",
		url: "get_individual",
		data: { "ticket": ticket, "uri": uri }
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText, function dateReviver (key, value) {
			return key === "data" && this.type === "Datetime" ? new Date(value) : value;
		});
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function put_individual(ticket, individual, callback) {
	var params = {
		type: "PUT",
		url: "put_individual",
		data: JSON.stringify({"ticket": ticket, "individual": individual}),
		contentType: "application/json"
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function get_property_value(ticket, uri, property_uri, callback) {
	var params = {
		type: "GET",
		url: "get_property_value",
		data: { "ticket": ticket, "uri": uri, "property_uri": property_uri }
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}

function execute_script(script, callback) {
	var params = {
		type: "POST",
		url: "execute_script",
		data: JSON.stringify({"script": script}),
		contentType: "application/json"
	};
	if(!callback) {
		params.async = false;
		var result = $.ajax(params);
		if (result.status >= 400) throw {status: result.status, description: result.statusText };
		return JSON.parse(result.responseText);
	}
	$.ajax(params)
		.fail( function () { throw {status: result.status, description: result.statusText } } )
		.done( function (data) { callback(data) } );
}
