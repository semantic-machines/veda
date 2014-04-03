function authenticate(login, password, callback) {
	$.ajax({
		type: "GET",
		url: "authenticate",
		data: { "login": login, "password": password },
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function is_ticket_valid(ticket, callback) {
	$.ajax({
		type: "GET",
		url: "is_ticket_valid",
		data: { "ticket": ticket },
		dataType: "json",
	})
	.fail( function () { callback(false) } )
	.done( function (data) { callback(data) } );
}

function query(ticket, query, callback) {
	$.ajax({
		type: "GET",
		url: "query",
		data: { "ticket": ticket, "query": query },
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function get_individuals(ticket, uris, callback) {
	$.ajax({
		//type: "GET",
		type: "POST",
		url: "get_individuals",
		//data: { "ticket": ticket, "uris": uris },
		data: JSON.stringify({ "ticket": ticket, "uris": uris }),
		contentType: "application/json",
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function get_individual(ticket, uri, callback) {
	$.ajax({
		type: "GET",
		url: "get_individual",
		data: { "ticket": ticket, "uri": uri },
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function put_individuals(ticket, individuals, callback) {
	$.ajax({
		type: "PUT",
		url: "put_individuals",
		data: JSON.stringify({"ticket": ticket, "individuals": individuals}),
		contentType: "application/json",
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function get_property_value(ticket, uri, property_uri, callback) {
	$.ajax({
		type: "GET",
		url: "get_property_value",
		data: { "ticket": ticket, "uri": uri, "property_uri": property_uri },
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function execute_script(script, callback) {
	$.ajax({
		type: "POST",
		url: "execute_script",
		data: JSON.stringify({"script": script}),
		contentType: "application/json",
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}
