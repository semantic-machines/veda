// Veda application Presenter

veda.Module(function AppPresenter(veda) { "use strict";
	
	// Prevent empty links routing
	$("body").on("click", "[href='']", function (e) {
		e.preventDefault();
	});
	
	// Route on link click
	$("body").on("click", "[href^='#/']", function (e) {
		e.preventDefault();
		var forced, 
			hash = $(this).attr("href");
		forced = (hash === location.hash ? false : true);
		return riot.route(hash, forced);
	});

	// Triggered in veda.init()
	veda.one("started", function () {
		var welcomeUri = (new veda.IndividualModel("cfg:Welcome"))["rdf:value"][0];
		var welcome = new veda.IndividualModel(welcomeUri);
		// Router function
		riot.route( function (hash) {
			var hash_tokens = hash.slice(2).split("/");
			var page = hash_tokens[0];
			var params = hash_tokens.slice(1);
			if (page !== "") {
				veda.load(page, params);
			} else {
				welcome.present("#main");
			}
		});
	});
	veda.on("started", function () {
		var layoutUri = (new veda.IndividualModel("cfg:Layout"))["rdf:value"][0];
		var layout = new veda.IndividualModel(layoutUri);
		layout.present("#app");
		riot.route(location.hash, true);
	});
	
	// Login invitation
	var loginTmpl = $("#login-template").html();
	var loginContainer = $("#login-holder");
	loginContainer.html(loginTmpl);
	var errorMsg = $("#login-error", loginContainer);	
	var submit = $("#submit", loginContainer);
	submit.click( function (e) {
		e.preventDefault();
		var authResult;
		try {
			errorMsg.addClass("hidden");
			// Successful authentication calls veda.init() in model
			authResult = veda.login( $("#login", loginContainer).val(), Sha256.hash( $("#password", loginContainer).val() ) );
			loginContainer.addClass("hidden");
			veda.trigger("login:success", authResult);
		} catch (ex1) {
			if (ntlm) {
				var params = {
					type: "POST",
					url: ntlmAddress + "ad/",
					data: {
						"login": $("#login", loginContainer).val(),
						"password": $("#password", loginContainer).val()
					},
					async: false
				};
				try {
					authResult = $.ajax(params);
					authResult = JSON.parse( authResult.responseText );
					loginContainer.addClass("hidden");
					veda.trigger("login:success", authResult);
					return;
				} catch (ex2) {}
			}
			errorMsg.removeClass("hidden");
			veda.trigger("login:failed");
		}		
	});

	// NTLM auth using iframe
	var ntlmProvider = new veda.IndividualModel("cfg:NTLMAuthProvider"),
		ntlm = ntlmProvider.hasValue("rdf:value"),
		iframe = $("<iframe>", {"class": "hidden"});
	if (ntlm) {
		var ntlmAddress = ntlmProvider["rdf:value"][0];
		iframe.appendTo(loginContainer);
	}

	veda.on("login:failed", function () {
		delCookie("user_uri"); 
		delCookie("ticket"); 
		delCookie("end_time");
		if (ntlm) {
			iframe.one("load", function () {
				try {
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
					}  else {
						loginContainer.removeClass("hidden");
					}
				} catch (e) {
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
		veda.user_uri = authResult.user_uri;
		veda.ticket = authResult.ticket;
		veda.end_time = authResult.end_time;
		setCookie("ticket", authResult.ticket, { path: "/", expires: new Date(parseInt(authResult.user_uri)) });
		setCookie("user_uri", authResult.user_uri, { path: "/", expires: new Date(parseInt(authResult.user_uri)) });
		setCookie("end_time", authResult.end_time, { path: "/", expires: new Date(parseInt(authResult.user_uri)) });
		veda.init();
	});

	veda.on("logout", function () {
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
	
	veda.on("error", function (error) {
		switch (error.status) {
			case 0:
				console.log ? console.log("Error:", JSON.stringify(error)) : null;
				$('#error-message').html("Операция не выполнена. Сервер недоступен. <br/> Пожалуйста, оставайтесь на этой странице и обратитесь в службу тех. поддержки. <br/><br/> Operation failed. Server is unavailable. <br> Please keep this page open and call support team.");
				$('#error-description').text( JSON.stringify(error) );
				$('#error-modal').modal('show');
				break;
			case 422:
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
});
