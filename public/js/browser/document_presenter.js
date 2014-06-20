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
					uri: "#/document/" + document["@"] 
				}
			) 
		);
		
		// Render document properties
		Object.getOwnPropertyNames(document.properties).map ( function (property_uri) {
			if (property_uri == "@") return;
			$("#document-properties", container).append(
				riot.render(
					document_single_property_template,
					{
						label: typeof document.properties[property_uri] == "object" ? document.properties[property_uri]["rdfs:label"].join(", ") : document.properties[property_uri],
						uri: typeof document.properties[property_uri] == "object" ? "#/document/" + document.properties[property_uri]["@"] : "",
						values: document[property_uri].map( function (item) {
							if (typeof item != "object") 
								// Check if string starts with http:// or ftp://
								return item.toString().search(/^.{3,5}:\/\//) == 0 ? "<a href='" + item + "'>" + item + "</a>" : item ;
							return "<a href='#/document/" + item["@"] + "'>" + (item["rdfs:label"] ? item["rdfs:label"].join(", ") : item["@"]) + "</a>";
						}).join(", ")
					}
				)
			);
		});

	});

});
