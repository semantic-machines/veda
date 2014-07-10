// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	var template = $("#current-user-template").html();
	var container = $("#nav-container #user-info");
	
	veda.on("user:loaded", function (user) {

		setTimeout(function(){

			// Render View
			container.html( 
				riot.render(
					template, 
					{	
						name: user["rdfs:label"]
								.filter(function(item){return item.language == veda.user.language || item.language == "NONE"}), 
						id: user["@"]
					}
				) 
			);
			
			$("#preferred-language > label", container).each( function() {
				if (this.id == user.language) $(this).addClass("active");
				$(this).on("click", function(){ 
					localize("body", this.id); 
					user.switch_language(this.id);

					// Refresh 'page'
					riot.route(location.hash, true);
				});
			});

			localize("body", user.language); 

		}, 0);
		
	});
	
});
