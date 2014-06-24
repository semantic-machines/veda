// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	veda.on("user:loaded", function (user) {

		setTimeout(function(){

			// Render View
			var template = $("#current-user-template").html();
			$("#nav-container #user-info").html( 
				riot.render(
					template, 
					{	
						name: user["rdfs:label"]
								.filter(function(item){return item.language == veda.user.language || item.language == "NONE"}), 
						id: user["@"]
					}
				) 
			);
			
		}, 0);
		
	});
	
});
