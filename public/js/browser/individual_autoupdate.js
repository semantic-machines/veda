/**

Autoupdate displayed individuals on client when they change on server

 */

veda.Module(function IndividualAutoupdate(veda) { "use strict";
	
	return;
	
	var socket;

	try {
		socket = new WebSocket("ws://echo.websocket.org");
	} catch (ex) {
		return socket = null;
	}
	
	socket.onmessage = function (event) {
		var msg = JSON.parse(event.data);
		console.log("received", event.data);
		for (var uri in msg) {
			var ind = new veda.IndividualModel(uri);
			ind.reset();
		}
	};
	
	var visible = (function (socket) {
		var counter = {};
		return {
			add: function (uri) {
				return counter[uri] ? ++counter[uri] : counter[uri] = 1;
			},
			remove: function (uri) {
				if (typeof counter[uri] === "undefined") return false;
				return ( typeof counter[uri] === "number" && counter[uri] === 1 ? delete counter[uri] : --counter[uri] );
			},
			subscribe: function () {
				setTimeout( function () {
					if (socket.readyState === 1) {
						socket.send( JSON.stringify(counter) );
					}
				}, 500);
			}
		}
	})(socket);

	veda.on("individual:loaded", function (individual, container, template, mode) {
		function displayedHandler(template) {
			visible.add(individual.id);
			template.one("remove", function () {
				visible.remove(individual.id);
			});
		}
		individual.one("individual:templateReady", displayedHandler);
		if (container === "#main") {
			visible.subscribe();
		}
	});

});
