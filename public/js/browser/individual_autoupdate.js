/**

Autoupdate displayed individuals on client when they change on server

 */

// TODO: optimize calls to websocket - combine subscribe/unsubscribe commands for several individuals

// New protocol

veda.Module(function IndividualAutoupdate(veda) { "use strict";

	var socket,
		address = "ws://" + location.hostname + ":8088/ccus";
		//address = "ws://echo.websocket.org";

	try {
		socket = new WebSocket(address);
	} catch (ex) {
		return socket = null;
	}

	// Handshake
	socket.onopen = function (event) {
		socket.send("ccus=" + veda.ticket);
	};

	socket.onclose = function (event) {
		veda.off("individual:loaded", updateWatch);
	};

	socket.onmessage = function (event) {
		try {
			var msg = event.data,
					uris;
			switch ( true ) {
				case ( msg.indexOf("=") === 0 ):
					// Synchronize subscription
					uris = msg.substr(1).split(",");
					for (var i = 0; i < uris.length; i++) {
						var tmp = uris[i].split("="),
								uri = tmp[0],
								updateCounter = tmp[1],
								list = subscription.get();
						list[uri] = list[uri] ? {
							displayCounter: list[uri].displayCounter,
							updateCounter: updateCounter
						} : {
							displayCounter: 1,
							updateCounter: updateCounter
						};
					}
				break;
				default:
					// Update individuals
					uris = msg.split(",");
					for (var i = 0; i < uris.length; i++) {
						var tmp = uris[i].split("="),
								uri = tmp[0],
								updateCounter = tmp[1],
								ind = new veda.IndividualModel(uri);
						if ( ind["v-s:updateCounter"][0] !== updateCounter ) {
							ind.reset();
						}
					}
				break;
			}
		} catch (e) {
			console.log("individual update service failed");
		}
	};

	//socket.onmessage = function (event) { console.log("ccus received:", event.data); };

	var subscription = (function (socket) {
		var list = {};
		return {
			get: function () {
				return list;
			},
			synchronize: function() {
				var msg = "=";
				socket.send(msg);
			},
			subscribe: function(uri) {
				setTimeout(function () {
					if (socket.readyState !== 1 || !uri) { return; }
					var displayCounter = list[uri] ? ++list[uri].displayCounter : 1 ;
					var updateCounter = (new veda.IndividualModel(uri))["v-s:updateCounter"][0] ;
					list[uri] = {
						displayCounter: displayCounter,
						updateCounter: updateCounter
					}
					var msg = "+" + uri + "=" + updateCounter;
					socket.send(msg);

					console.log("subscribe", msg, list);

				}, 1000);
			},
			unsubscribe: function (uri) {
				if (socket.readyState !== 1) { return };
				if (uri === "*") {
					list = {};
				} else {
					if ( !list[uri] ) {
						return;
					} else if ( list[uri].displayCounter === 1 ) {
						delete list[uri];
					} else {
						--list[uri].displayCounter;
						return;
					}
					var msg = "-" + uri;
					socket.send(msg);

					console.log("unsubscribe", msg, list);

				}
			},
		}
	})(socket);

	veda.on("individual:loaded", updateWatch);

	function updateWatch(individual) {
		individual.one("individual:templateReady", subscribeDisplayed);
	}

	function subscribeDisplayed(template) {
		var individual = this;
		subscription.subscribe(individual.id);

		template.one("remove", function () {
			subscription.unsubscribe(individual.id);
		});
	}
});


