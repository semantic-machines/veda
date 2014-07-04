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

		// Render document properties (from classes)
		var props = {};
		
		Object.getOwnPropertyNames(document.properties).reduce( function (acc, property_uri) {
			acc[property_uri] = undefined;
			return acc;
		}, props);
		
		function getTypes(individual, types) {
			if (!individual["rdfs:subClassOf"]) return;
			individual["rdfs:subClassOf"].reduce(function (acc, _class) {
				getTypes(_class, acc);
				acc[_class["@"]] = new ClassModel(veda, [_class["@"]]);
				return acc;
			}, types);
		}
		
		var types = {};
		
		document["rdf:type"].map(function(_class){ 
			types[_class["@"]] = new ClassModel(veda, [_class["@"]]);
			getTypes(_class, types) 
		});
		
		types["rdfs:Resource"] = new ClassModel(veda, ["rdfs:Resource"]);
		
		Object.getOwnPropertyNames(types).map( function (type) {
			var acc=0;
			var el = $("<div>");
			Object.getOwnPropertyNames(types[type].domainProperties).map( function (property_uri) {
				try {
					renderProperty(veda, document, property_uri, document_single_property_template, el);
					acc++;
					props[property_uri] = "rendered";
				} catch (e) {
					return;
				}
			});
			if (acc) { 
				$("#document-properties", container).append(el);
				el.prepend("<h5 class='text-muted text-right'>" 
					+ 
					types[type]["rdfs:label"]
						.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
						.join(", ")
					+ "</h5>"
				);
				el.prepend("<hr>");
			}
		});
		
		// Render rest document properties
		var acc=0;
		var el = $("<div>");
		
		Object.getOwnPropertyNames(props).map ( function (property_uri) {
			if (property_uri == "@") return;
			if (props[property_uri] == "rendered") return;
			try {
				renderProperty(veda, document, property_uri, document_single_property_template, el);
				acc++;
			} catch (e) {
				return;
			}
		});
		if (acc) {
			$("#document-properties", container).append(el);
			el.prepend("<hr>");
		}
		
	});

});
