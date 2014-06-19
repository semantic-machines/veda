// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";

	// Get templates
	var document_template = $("#document-template").html();
	var document_single_property_template = $("#document-single-property-template").html();
	var document_label_template = $("#document-label-template").html();
	
	veda.on("document:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		container.html(document_template);

		// Render document title		
		$("#label", container).html( 
			riot.render(
				document_label_template,
				{ 
					label: document["rdfs:label"] ? document["rdfs:label"].join(", ") : document["@"],
					uri: document["@"] 
				}
			) 
		);
		
		// Render document properties
		Object.getOwnPropertyNames(document.properties).map ( function (property_uri) {
			console.log("property", property_uri);
			if (property_uri == "@") return;
			$("#document-properties", container).append(
				riot.render(
					document_single_property_template,
					{
						label: document.properties[property_uri]["rdfs:label"] ? document.properties[property_uri]["rdfs:label"].join(", ") : property_uri,
						uri: document.properties[property_uri]["@"],
						values: document[property_uri].map( function (item) {
							console.log("value", item);
							if (typeof item != "object") return item;
							return "<a href='#/document/" + item["@"] + "'>" + (item["rdfs:label"] ? item["rdfs:label"].join(", ") : item["@"]) + "</a>";
						}).join(", ")
					}
				)
			);
		});

	});

});
