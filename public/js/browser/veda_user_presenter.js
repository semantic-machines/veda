// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {

		// Render View
		var template = $("#current-user-template").html();
		$("#current-user").html( riot.render(template, {name: user.individual["rdfs:label"][0].data}) );
	
	});

});
