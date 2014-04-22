// User Presenter

veda(function UserPresenter(app) { "use strict";

	app.on("user:loaded", function (params) {
		// Get or create Model
		var user = app.user || app.RegisterModule(new app.UserModel(), app, "user");
	
		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
	
		// Listen View changes & update Model
		$("#document #load").on("click", function(event) {
			event.preventDefault();
		});
		$("#document #save").on("click", function(event) {
			event.preventDefault();
		});
	
		// Listen Model changes & update View
		user.on("user:loaded", function() {
		});
		user.on("user:saved", function() {
		});

	});

});
