// Veda application Presenter

veda(function VedaPresenter(app) { "use strict";

	app.on("ready", function() {

		function showWellcome() { 
			var template = $("#wellcome").html();
			$("#main").html(template);
		};

		// Listen to quit && authentication failure events
		app.on("auth:quit auth:failed", function () {
			var template = $("#login-template").html();
			$("#main").html(template);		

			$("#login-form #submit").on("click", function(event) {
				event.preventDefault();
				app.authenticate( $("#login-form #login").val(), Sha256.hash( $("#login-form #password").val() ) );
			});
		});

		// Listen to authentication success event
		app.on("auth:success", function (user_uri, ticket, end_time ) {
			setCookie("user_uri", user_uri, {path: "/", expires: end_time});
			setCookie("ticket", ticket, {path: "/", expires: end_time});
			showWellcome();
		});

		// Router function
		riot.route( function (hash) {
			var hash_tokens = hash.slice(2).split("/");
			var page = hash_tokens[0];
			var params = hash_tokens.slice(1);
			page == "console" 	? 	app.trigger("load:console") : 
			page == "document"	? 	app.trigger("load:document") :
			page == "search"	? 	app.trigger("load:search") : 
									showWellcome(); // Default wellcome view
		});

		// Listen to a link click and call router
		$("body").on("click", "[href^='#/']", function(e) {
			e.preventDefault();
			var link = $(this);
			return riot.route($(this).attr("href"));
		});

		// If ticket not in cookies or expired show login form
		if ( !app.ticket || !app.user_uri || !is_ticket_valid(app.ticket) ) {
			app.trigger("auth:quit"); 
			return;
		} 

		// Route to current location hash
		riot.route (location.hash);
	});

});
