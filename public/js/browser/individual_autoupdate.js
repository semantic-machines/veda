/**

Autoupdate displayed individuals on client when they change on server

 */

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
		var msg = event.data,
				uris;
		console.log("server:", msg);
		switch ( true ) {
			case ( msg.indexOf("=") === 0 ):
				// Synchronize subscription
				uris = msg.substr(1).split(",");
				for (var i = 0; i < uris.length; i++) {
					var tmp = uris[i].split("="),
							uri = tmp[0],
							updateCounter = parseInt(tmp[1]),
							list = subscription.get();
					list[uri] = list[uri] ? {
						subscribeCounter: list[uri].subscribeCounter,
						updateCounter: updateCounter
					} : {
						subscribeCounter: 1,
						updateCounter: updateCounter
					};
				}
			break;
			default:
				try {
					// Update individuals
					uris = msg.split(",");
					for (var i = 0; i < uris.length; i++) {
						var tmp = uris[i].split("="),
								uri = tmp[0],
								updateCounter = parseInt(tmp[1]),
								individual = new veda.IndividualModel(uri),
								list = subscription.get();
						list[uri] = list[uri] ? {
							subscribeCounter: list[uri].subscribeCounter,
							updateCounter: updateCounter
						} : {
							subscribeCounter: 1,
							updateCounter: updateCounter
						};
						if ( !individual.hasValue("v-s:updateCounter") || individual["v-s:updateCounter"][0] !== updateCounter ) {
							individual.reset();
						}
					}
				} catch (e) {
					console.log("error: individual update service failed");
				}
			break;
		}
	};

	//socket.onmessage = function (event) { console.log("server:", event.data); };

	var subscription = (function (socket) {
		var list = {},
				delta = {},
				interval,
				delay = 1000,
				last;

		function pushDelta() {
			var subscribe = [],
					unsubscribe = [],
					subscribeMsg,
					unsubscribeMsg;
			for (var uri in delta) {
				if (delta[uri].operation === "+") {
					subscribe.push("+" + uri + "=" + delta[uri].updateCounter);
				} else {
					unsubscribe.push("-" + uri);
				}
			}
			subscribeMsg = subscribe.join(",");
			unsubscribeMsg = unsubscribe.join(",");
			delta = {};
			if (subscribeMsg) {
				socket.send(subscribeMsg);
				console.log("client: subscribe", subscribeMsg);
			}
			if (unsubscribeMsg) {
				socket.send(unsubscribeMsg);
				console.log("client: unsubscribe", unsubscribeMsg);
			}

			clearInterval(interval);
			interval = undefined;
		}

		return {
			get: function () {
				return list;
			},
			synchronize: function() {
				clearInterval(interval);
				interval = undefined;
				list = {};
				delta = {};
				socket.send("=");
				console.log("client: synchronize");
			},
			subscribe: function(uri) {
				if (list[uri]) {
					++list[uri].subscribeCounter;
					return;
				}
				var individual = new veda.IndividualModel(uri);
				var updateCounter = individual.hasValue("v-s:updateCounter") ? individual["v-s:updateCounter"][0] : 0;
				list[uri] = {
					subscribeCounter: 1,
					updateCounter: updateCounter
				};
				delta[uri] = {
					operation: "+",
					updateCounter: updateCounter
				};
				if (!interval) {
					interval = setInterval(pushDelta, delay);
				}
			},
			unsubscribe: function (uri) {
				if (uri === "*") {
					clearInterval(interval);
					interval = undefined;
					list = {};
					delta = {};
					socket.send("-*");
					console.log("client: unsubscribe all");
				} else {
					if ( !list[uri] ) {
						return;
					} else if ( list[uri].subscribeCounter === 1 ) {
						delete list[uri];
						delta[uri] = {
							operation: "-"
						};
						if (!interval) {
							interval = setInterval(pushDelta, delay);
						}
					} else {
						--list[uri].subscribeCounter;
						return;
					}
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

	veda.updateSubscription = subscription;

});
