// User Presenter

veda.Module(function UserPresenter(veda) { "use strict";

	var userTemplate = $("#user-template").html();
	var languageTemplate = $("#language-template").html();
	var userInfo = $("#user-info");
	var languageSelector = $("#preferred-language");
	
	veda.on("started", function () {

		// Render languages
		var languages = "";
		Object.keys(veda.user.availableLanguages).map ( function (language_uri) {
			languages += riot.render(languageTemplate, veda.user.availableLanguages[language_uri]);
		});
		languageSelector.html(languages);
		
		// Render user
		userInfo.empty();
		var userTmpl = new veda.IndividualModel("v-ui:IconPersonTemplate");
		veda.user.present(userInfo, userTmpl);

		var $languages = $("label", languageSelector);
		$languages.each( function() {
			if (this.id in veda.user.language) $(this).addClass("active");
			$(this).on("click", function() { 
				veda.user.toggleLanguage(this.id);
				this.id in veda.user.language ? $(this).addClass("active") : $(this).removeClass("active");
			});
		});

	});
	
});
