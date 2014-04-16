// Document Presenter

function DocumentPresenter() { "use strict";
	// Get or create Model
	var doc = app.document || RegisterModule(new DocumentModel(), app, "document");

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
