// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {
	
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
		document.on("document:loaded", function() {
		});
		document.on("document:saved", function() {
		});

	});

});
