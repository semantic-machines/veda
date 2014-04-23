// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {

		// Listen to logout click
		$("#logout").on("click", function(e) {
			e.preventDefault();
			return veda.quit();
		});

		// Listen to logout click
		$("#set-trace").on("click", function(e) {
			var $el = $(this);
			e.preventDefault();
			if ($el.hasClass("active")) { 
				set_trace(0, false);
				$el.removeClass("active");
				return;
			}
			set_trace(0, true);
			$el.addClass("active");
		});

		// Render View
		var template = $("#current-user-template").html();
		$("#current-user").html( riot.render(template, {name: user.individual["rdfs:label"][0].data}) );
		
	});

});
