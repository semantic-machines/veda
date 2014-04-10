// Veda application Presenter

function VedaPresenter(hash) { "use strict";

	// Get or create the application Model
	window.app = window.app || Module(new VedaModel(), window.app, "veda");

	// Listen to a link click and call router
	var links = $("a");
	links.click(function() {
		return $.route($(this).attr("href"));
	});

	// Router function to call appropriate Presenter
	$.route(function(hash) {
		var hash_tokens = hash.slice(2).split("/");
		var page = hash_tokens[0];
		var params = hash_tokens.slice(1);
		page == "console" 	? 	ConsolePresenter(params) : 
		page == "document"	? 	DocumentPresenter(params) :
		page == "search"		? 	SearchPresenter(params) : 
										ConsolePresenter(params); // Default Presenter
	});

	// Call router for current browser location hash
	$.route(hash);
};
