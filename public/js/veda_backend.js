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

function query(ticket, query, level, callback) {
	$.ajax({
		type: "GET",
		url: "query",
		data: { "ticket": ticket, "query": query, "level": level },
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function get_individual(ticket, uri, level, callback) {
	$.ajax({
		type: "GET",
		url: "individual",
		data: { "ticket": ticket, "uri": uri, "level": level },
		dataType: "json"
	})
	.fail( function () { callback(null) } )
	.done( function (data) { callback(data) } );
}

function put_individual(ticket, uri, individual, callback) {
	$.ajax({
		type: "PUT",
		url: "individual",
		data: { "ticket": ticket, "uri": uri, "individual": individual },
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
