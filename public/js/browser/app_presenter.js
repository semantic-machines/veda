// Veda application Presenter

veda.Module(function AppPresenter(veda) { "use strict";

	// Prevent empty links routing
	$("body").on("click", "[href='']", function (e) {
		e.preventDefault();
	});

	// App loading indicator
	var appLoadIndicator = $("#app-load-indicator");
	veda.on("init:progress", function (progress) {
		if (progress !== 100) {
			appLoadIndicator.removeClass("hidden");
		} else {
			appLoadIndicator.addClass("hidden");
		}
	});

	// Triggered in veda.init()
	veda.one("started", function () {
		var welcomeUri = (new veda.IndividualModel("cfg:Welcome"))["rdf:value"][0],
			welcome = new veda.IndividualModel(welcomeUri);
		// Router function
		riot.route( function (hash) {
			if ( !hash ) { return welcome.present("#main"); }
			if ( hash.indexOf("#/") < 0 ) { return; }
			var hash_tokens = hash.slice(2).split("/");
			var page = hash_tokens[0];
			var params = hash_tokens.slice(1);
			page ? veda.load(page, params) : welcome.present("#main");
		});
	});
	veda.on("started", function () {
		var layoutUri = (new veda.IndividualModel("cfg:Layout"))["rdf:value"][0];
		var layout = new veda.IndividualModel(layoutUri);
		layout.present("#app");
		riot.route(location.hash, true);
	});

	// Error handling
	veda.on("error", function (error) {
		switch (error.status) {
			case 0:
				console.log ? console.log("Error:", JSON.stringify(error)) : null;
				$('#error-message').html("Операция не выполнена. Сервер недоступен. <br/> Пожалуйста, оставайтесь на этой странице и обратитесь в службу тех. поддержки. <br/><br/> Operation failed. Server is unavailable. <br> Please keep this page open and call support team.");
				$('#error-description').text( JSON.stringify(error) );
				$('#error-modal').modal('show');
				break;
			case 422:
				console.log ? console.log("Error:", JSON.stringify(error)) : null;
				break;
			case 429:
				console.log ? console.log("Error:", JSON.stringify(error)) : null;
				$('#error-message').html("Операция не выполнена. Данные не сохранены. <br/> Пожалуйста,  оставайтесь на этой странице и обратитесь в службу тех. поддержки. <br/><br/> Operation failed. Data wasn't saved. <br/> Please keep this page open and call support team.");
				$('#error-description').text( JSON.stringify(error) );
				$('#error-modal').modal('show');
				break;
			case 471:
				veda.logout();
				break;
			case 472:
				console.log ? console.log("Error:", JSON.stringify(error)) : null;
				break;
			case 473:
				break; // handled in login screen
			default:
				$('#error-message').html("Операция не выполнена. <br/> Пожалуйста, оставайтесь на этой странице и обратитесь в службу тех. поддержки. <br/><br/> Operation failed. <br/> Please keep this page open and call support team.");
				$('#error-description').text( JSON.stringify(error) );
				$('#error-modal').modal('show');
				console.log ? console.log("Error:", JSON.stringify(error)) : null;
		}
	});

	// Login invitation
	var loginTmpl = $("#login-template").html();
	var loginContainer = $("#login-container");
	loginContainer.html(loginTmpl);
	var errorMsg = $("#login-error", loginContainer);
	var submit = $("#submit", loginContainer);
	submit.click( function (e) {
		e.preventDefault();
		var login = $("#login", loginContainer).val(),
			password = $("#password", loginContainer).val(),
			hash = Sha256.hash(password),
			authResult;
		try {
			errorMsg.addClass("hidden");
			authResult = veda.login(login, hash);
			veda.trigger("login:success", authResult);
		} catch (ex1) {
			if (ntlm) {
				var params = {
					type: "POST",
					url: ntlmAddress + "ad/",
					data: {
						"login": login,
						"password": password
					},
					async: false
				};
				try {
					authResult = $.ajax(params);
					authResult = JSON.parse( authResult.responseText );
					veda.trigger("login:success", authResult);
					return;
				} catch (ex2) {}
			}
			errorMsg.removeClass("hidden");
			veda.trigger("login:failed");
		}
	});

	// NTLM auth using iframe
	var ntlmProvider = new veda.IndividualModel("cfg:NTLMAuthProvider", undefined, undefined, undefined, true, false),
		ntlm = ntlmProvider.properties["rdf:value"] && ntlmProvider.properties["rdf:value"].length,
		iframe = $("<iframe>", {"class": "hidden"});
	if (ntlm) {
		var ntlmAddress = ntlmProvider.properties["rdf:value"][0].data;
		iframe.appendTo(loginContainer);
	}

	veda.on("login:failed", function () {
		$("#app").empty();
		delCookie("user_uri");
		delCookie("ticket");
		delCookie("end_time");
		if (ntlm) {
			iframe.one("load", function () {
				try {
					loginContainer.addClass("hidden");
					var body = iframe.contents().find("body"),
						ticket = $("#ticket", body).text(),
						user_uri = $("#user_uri", body).text(),
						end_time = $("#end_time", body).text(),
						authResult = {
							ticket: ticket,
							user_uri: user_uri,
							end_time: end_time
						};
					if (ticket && user_uri && end_time) {
						veda.trigger("login:success", authResult);
					} else {
						throw "Not authenticated";
					}
				} catch (ex) {
					loginContainer.removeClass("hidden");
				}
			});
			document.domain = document.domain;
			iframe.attr("src", ntlmAddress);
		} else {
			loginContainer.removeClass("hidden");
		}
	});

	// Initialize application if ticket is valid
	veda.on("login:success", function (authResult) {
		loginContainer.addClass("hidden");
		veda.user_uri = authResult.user_uri;
		veda.ticket = authResult.ticket;
		veda.end_time = authResult.end_time;
		setCookie("ticket", authResult.ticket, { path: "/", expires: new Date(parseInt(authResult.user_uri)) });
		setCookie("user_uri", authResult.user_uri, { path: "/", expires: new Date(parseInt(authResult.user_uri)) });
		setCookie("end_time", authResult.end_time, { path: "/", expires: new Date(parseInt(authResult.user_uri)) });
		veda.init();
	});

	// Logout handler
	veda.on("logout", function () {
		$("#app").empty();
		delCookie("ticket");
		delCookie("user_uri");
		delCookie("end_time");
		loginContainer.removeClass("hidden");
	});

	// Check if ticket in cookies is valid
	var ticket = getCookie("ticket") == "undefined" ? undefined : getCookie("ticket"),
		user_uri = getCookie("user_uri") == "undefined" ? undefined : getCookie("user_uri"),
		end_time = getCookie("end_time") == "undefined" ? undefined : getCookie("end_time");

	if ( ticket && user_uri && end_time && is_ticket_valid(ticket) ) {
		veda.trigger("login:success", {
			ticket: ticket,
			user_uri: user_uri,
			end_time: end_time
		});
	} else {
		veda.trigger("login:failed");
	}

});
