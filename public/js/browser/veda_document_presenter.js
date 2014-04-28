// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
	
		$("#document #doc").html(JSON.stringify(document.individual));
		
	});

});
