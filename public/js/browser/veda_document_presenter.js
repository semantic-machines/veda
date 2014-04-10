// Document Presenter

function DocumentPresenter() { "use strict";
	// Get View
	var template = $("#document-template").html();

	// Get or create Model
	var doc = app.document || Module(new DocumentModel(), app, "document");

	doc.on("show", function() {
		$("#main").html(template);
	});
	doc.trigger("show");

	// listen browser events
	$("#document #load").on("click", 
		function(event) {
			event.preventDefault();
		});
	$("#document #save").on("click", 
		function(event) {
			event.preventDefault();
		});

	// listen Model events
	doc.on("loaded", function() {
	});
	doc.on("saved", function() {
	});
};
