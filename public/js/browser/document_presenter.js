// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
		
		var single_property = $("#single-property-template").html();
	
		for (var i in document.flat_individual) {
			$("#document #individual").append( riot.render(single_property, {property: i, value: document.flat_individual[i]}) );
		}
		
	});

});
