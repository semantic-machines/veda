// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {
	
		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
	
		//$("#document #doc").html(JSON.stringify(document.flat_individual));
		
		var tmpl = $("#single-property-template").html();
		for (var property in document.flat_individual) {
			$("#document #doc").append(
				riot.render(tmpl, { property: property, value: document.flat_individual[property] })
			);
		}
		
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
