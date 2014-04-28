// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {

		// Render View
		var template = $("#current-user-template").html();
		$("#nav-container #user-info").html( riot.render(template, {name: user.individual["rdfs:label"][0].data}) );
		
		// Listen to logout click
		$("#logout").on("click", function(e) {
			e.preventDefault();
			$("#current-user").html("");
			veda.trigger("auth:quit");
		});

	});

});
