// Document Presenter
Veda(function DocumentPresenter(veda) { "use strict";

	// Get templates
	var document_template = $("#document-template").html();
	var document_single_property_template = $("#document-single-property-template").html();
	var document_label_template = $("#document-label-template").html();
	
	veda.on("document:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		container.html(document_template);
		localize(container, veda.user.language);
		
		// Render document title
		$("#document-label", container).html( 
			riot.render(
				document_label_template,
				{ 
					label: document["rdfs:label"] ? document["rdfs:label"]
						.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
						.join(", ") : document["@"],
					uri: document["@"] 
				}
			) 
		);

		// Render document properties (respecting classes)
		var properties = {};
		Object.getOwnPropertyNames(document.properties).reduce( function (accumulator, property_uri) {
			accumulator[property_uri] = undefined;
			return accumulator;
		}, properties);

		// Loop through classes and render domain properties 
		Object.getOwnPropertyNames(document.classTree.classes).map( function (_class) {
			var counter=0;
			var el = $("<div>");
			Object.getOwnPropertyNames(document.classTree.classes[_class].domainProperties).map( function (property_uri) {
				try {
					renderProperty(veda, document, property_uri, document_single_property_template, el);
					counter++;
					properties[property_uri] = "rendered";
				} catch (e) {
					return;
				}
			});
			if (counter) { 
				$("#document-properties", container).append(el);
				el.prepend("<h5 class='text-muted text-right'>" 
					+ 
					document.classTree.classes[_class]["rdfs:label"]
						.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
						.join(", ")
					+ "</h5>"
				);
				el.prepend("<hr>");
			}
		});
		
		// Render rest document properties
		var counter=0;
		var el = $("<div>");
		
		Object.getOwnPropertyNames(properties).map ( function (property_uri) {
			if (property_uri == "@") return;
			if (properties[property_uri] == "rendered") return;
			try {
				renderProperty(veda, document, property_uri, document_single_property_template, el);
				counter++;
			} catch (e) {
				return;
			}
		});
		if (counter) {
			$("#document-properties", container).append(el);
			el.prepend("<hr>");
		}
		
		$("#edit-document", container).on("click", function (){
		
		});
		
	});

});
