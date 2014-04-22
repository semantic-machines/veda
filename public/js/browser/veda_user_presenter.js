// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {
		// Get or create Model
		//var user = veda.user || RegisterModule(new UserModel(), veda, "user");
	
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
