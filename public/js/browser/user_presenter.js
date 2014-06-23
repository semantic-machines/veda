// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {

		// Render View
		var template = $("#current-user-template").html();
		$("#nav-container #user-info").html( riot.render(template, {name: user["rdfs:label"][0], id: user["@"]}) );

	});

});
