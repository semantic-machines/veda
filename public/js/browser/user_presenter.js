// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	var template = $("#current-user-template").html();
	var languageTemplate = $("#language-template").html();
	var container = $("#nav-container #user-info");
	
	veda.on("user:loaded", function (user) {

		setTimeout ( function () {
			
			container.hide();

			// Render languages
			var languages = ""
			Object.keys(veda.availableLanguages).map ( function (language_uri) {
				languages += riot.render(languageTemplate, veda.availableLanguages[language_uri]);
			});
			
			// Render user
			container.html( riot.render(template, {user: user, languages: languages}) );
			
			$("#preferred-language > label", container).each( function() {
				if (this.id in user.language) $(this).addClass("active");
				$(this).on("click", function() { 
					user.toggleLanguage(this.id);
				});
			});

			container.fadeIn(250);

		}, 10);
		
	});
	
});
