// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
		
		var single_property = $("#single-property-template").html();
	
		var doc = document.expanded_localized;
		for (var i in doc) {
			if (i == "@") continue;
			console.log(doc[i]["property"]);
			$("#document #doc").append( 
				riot.render(
					single_property, 
					{ 
						property: JSON.stringify(doc[i]["property"]["rdfs:label"]), //[0]["data"]),
						values: JSON.stringify(doc[i]["values"])
					}
				) 
			);
		}

	});

});
