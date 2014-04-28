// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
	
		//$("#document #doc").html(JSON.stringify(document.individual));
		
		var tmpl = $("#single-property-template").html();
		for (var property in document.flat_individual) {
			$("#document #doc").append(
				riot.render(tmpl, { property: property, value: document.flat_individual[property] })
			);
		}

	});

});
