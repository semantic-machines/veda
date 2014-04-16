// Document Presenter

veda.DocumentPresenter = function() { "use strict";
	// Get or create Model
	var doc = veda.document || veda.RegisterModule(new veda.DocumentModel(), veda, "document");

	// Get Template
	var template = $("#document-template").html();
	
	// Render View
	$("#main").html(template);

	// listen browser events
	$("#document #load").on("click", function(event) {
		event.preventDefault();
	});
	$("#document #save").on("click", function(event) {
		event.preventDefault();
	});

	// listen Model events
	doc.on("loaded", function() {
	});
	doc.on("saved", function() {
	});
};
