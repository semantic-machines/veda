// User Presenter

Veda(function UserPresenter(veda) { "use strict";

	var template = $("#current-user-template").html();
	var languageTemplate = $("#language-template").html();
	var container = $("#nav-container #user-info");
	
	veda.on("started", function () {

		container.hide();

		// Render languages
		var languages = ""
		Object.keys(veda.availableLanguages).map ( function (language_uri) {
			languages += riot.render(languageTemplate, veda.availableLanguages[language_uri]);
		});
		
		// Render user
		container.html( riot.render(template, {user: veda.user, languages: languages}) );
		
		var $languages = $("#preferred-language > label", container);
		$languages.each( function() {
			if (this.id in veda.user.language) $(this).addClass("active");
			$(this).on("click", function() { 
				veda.user.toggleLanguage(this.id);
				this.id in veda.user.language ? $(this).addClass("active") : $(this).removeClass("active");
			});
		});

		container.fadeIn(250);

	});
	
});
