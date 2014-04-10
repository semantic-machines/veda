// Veda application Presenter

function VedaPresenter(hash) { "use strict";

	// Listen to a link click && route to appropriate Presenter
	var links = $("a");
	links.click(function() {
		return $.route($(this).attr("href"));
	});

	// Router
	$.route(function(hash) {
		var hash_tokens = hash.slice(2).split("/");
		var page = hash_tokens[0];
		var params = hash_tokens.slice(1);
		page == "console" 	? ConsolePresenter(params) : 
		page == "document"	? DocumentPresenter(params) :
		page == "search"		? SearchPresenter(params) : VedaPresenter(params);
	});

	// Get or create the application Model
	var app = app || Module(new VedaModel(), app, "veda");
	hash ? $.route(hash) : ConsolePresenter();
}