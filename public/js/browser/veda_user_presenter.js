// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {

		// Listen to logout click
		$("#logout").on("click", function(e) {
			e.preventDefault();
			$("#current-user").html("");
			return veda.quit();
		});

		// Render View
		var template = $("#current-user-template").html();
		$("#current-user").html( riot.render(template, {name: user.individual["rdfs:label"][0].data}) );
		
	});

});
