// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	var template = $("#current-user-template").html();
	var container = $("#nav-container #user-info");
	
	veda.on("user:loaded", function (user) {

		setTimeout(function(){

			// Render View
			container.html( 
				riot.render( template, user, function localizeData(data) {
					return data instanceof Array ? 
						data.filter(function(i){return !(i instanceof String) ? true : i.language == Veda().user.language || i.language == "NONE" }).join(", ") 
						:
						data;
				})
			);
			
			$("#preferred-language > label", container).each( function() {
				if (this.id == user.language) $(this).addClass("active");
				$(this).on("click", function(){ 
					user.switch_language(this.id);
					veda.trigger("language:changed");
				});
			});

		}, 10);
		
	});
	
});
