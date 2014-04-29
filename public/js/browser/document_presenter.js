// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	veda.on("document:loaded", function (document) {

		// Render View
		var template = $("#document-template").html();
		$("#main").html(template);
		
		var single_property = $("#single-property-template").html();
	
		var doc = document.flat_individual;
		for (var i in document.flat_individual) {
			if (i == "@") continue;
			$("#document #doc").append( 
				riot.render(
					single_property, 
					{ 
						property_uri: doc[i]["property_uri"], 
						property_label: doc[i]["property_label"], 
						property_range: doc[i]["property_range"][0]["data"], 
						property_values: doc[i]["property_values"]
					}
				) 
			);
		}

		//$("#document #ind").html( "<pre>" + JSON.stringify(localized, true, 2) + "</pre>");

	});

});
