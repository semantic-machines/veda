// Document Presenter

Veda.prototype.DocumentPresenter  = function DocumentPresenter() { "use strict";

	// Get or create Model
	var doc = veda.document || veda.RegisterModule(new veda.DocumentModel(), veda, "document");

	// Render View
	var template = $("#document-template").html();
	$("#main").html(template);

	// Listen View changes & update Model
	$("#document #load").on("click", function(event) {
		event.preventDefault();
	});
	$("#document #save").on("click", function(event) {
		event.preventDefault();
	});

	// Listen Model changes & update View
	doc.on("loaded", function() {
	});
	doc.on("saved", function() {
	});
};
