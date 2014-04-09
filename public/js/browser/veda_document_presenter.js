// Document Presenter
$(function() { "use strict";
	// get Model
	var doc = app.document;
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
});