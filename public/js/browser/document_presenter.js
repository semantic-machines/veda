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

		/*
		// Render document properties
		Object.getOwnPropertyNames(document.properties).map ( function (property_uri) {
			if (property_uri == "@") return;
			var label, uri, values;
			label = typeof document.properties[property_uri] == "object" ? 
						document.properties[property_uri]["rdfs:label"]
							.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
							.join(", ")
						: document.properties[property_uri];
			uri = typeof document.properties[property_uri] == "object" ? document.properties[property_uri]["@"] : "";
			values = document[property_uri]
						.map( function (item) {
							if (item instanceof String)
								// Check if string starts with http:// or ftp://
								return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
							else if (item instanceof IndividualModel)
								return "<a href='#/document/" + item["@"] + "'>" + 
									(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
							else return item;
						})
						.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
						.join(", ");
			$("#document-properties", container).append(
				riot.render(
					document_single_property_template,
					{
						label: label,
						uri: uri,
						values: values
					}
				)
			);
		});
		*/
		
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

		//$("#document-properties", container).append("<hr>");
		
		types["rdfs:Resource"] = new ClassModel(veda, ["rdfs:Resource"]);
		
		Object.getOwnPropertyNames(types).map( function (type) {
			var acc=0;
			var el = $("<div>");
			Object.getOwnPropertyNames(types[type].domainProperties).map( function (property_uri) {
				try {
					var label, uri, values;
					label = typeof document.properties[property_uri] == "object" ? 
								document.properties[property_uri]["rdfs:label"]
									.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
									.join(", ")
								: document.properties[property_uri];
					uri = typeof document.properties[property_uri] == "object" ? document.properties[property_uri]["@"] : "";
					values = document[property_uri]
								.map( function (item) {
									if (item instanceof String)
										// Check if string starts with http:// or ftp://
										return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
									else if (item instanceof IndividualModel)
										return "<a href='#/document/" + item["@"] + "'>" + 
											(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
									else return item;
								})
								.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
								.join(", ");
					el.append(
						riot.render(
							document_single_property_template,
							{
								label: label,
								uri: uri,
								values: values
							}
						)
					);
					acc++;
					props[property_uri] = "rendered";
				} catch (e) {
					return;
				}
			});
			if (acc) { 
				$("#document-properties", container).append(el);
				el.prepend("<h4>" 
					+ 
					types[type]["rdfs:label"]
						.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
						.join(", ")
					+ "</h4>"
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
			var label, uri, values;
			label = typeof document.properties[property_uri] == "object" ? 
						document.properties[property_uri]["rdfs:label"]
							.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
							.join(", ")
						: document.properties[property_uri];
			uri = typeof document.properties[property_uri] == "object" ? document.properties[property_uri]["@"] : "";
			values = document[property_uri]
						.map( function (item) {
							if (item instanceof String)
								// Check if string starts with http:// or ftp://
								return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
							else if (item instanceof IndividualModel)
								return "<a href='#/document/" + item["@"] + "'>" + 
									(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
							else return item;
						})
						.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
						.join(", ");
			el.append(
				riot.render(
					document_single_property_template,
					{
						label: label,
						uri: uri,
						values: values
					}
				)
			);
			acc++;
		});
		if (acc) {
			$("#document-properties", container).append(el);
			el.prepend("<h4>" + "Без типа" + "</h4>");
			el.prepend("<hr>");
		}
		
	});

});
