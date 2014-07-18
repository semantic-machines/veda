// Document Presenter
Veda(function DocumentPresenter(veda) { "use strict";

	function renderDocumentProperty (veda, individual, property_uri, template, container) {
		var label, uri, values;
		label = typeof individual.properties[property_uri] == "object" ? 
					individual.properties[property_uri]["rdfs:label"]
						.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
						.join(", ")
					: individual.properties[property_uri];
		uri = typeof individual.properties[property_uri] == "object" ? individual.properties[property_uri]["@"] : "";
		values = individual[property_uri]
					.map( function (item) {
						if (item instanceof String)
							// Check if string starts with http:// or ftp://
							return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
						else if (item instanceof IndividualModel)
							return "<a data-toggle='popover' href='#/document/" + item["@"] + "'>" + 
								(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
						else return item;
					})
					.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
					.join(", ");
		container.append(
			riot.render(
				template,
				{
					label: label,
					uri: uri,
					values: values
				}
			)
		);
	}

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

		Object.getOwnPropertyNames(document.classTree.roots).map( function (class_uri) {
			(function renderClassProperties (_class) {
				if (!_class.rendered) {
					var counter=0;
					var el = $("<div>");
					Object.getOwnPropertyNames(_class.domainProperties).map( function (property_uri) {
						try {
							renderDocumentProperty(veda, document, property_uri, document_single_property_template, el);
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
							_class["rdfs:label"]
								.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
								.join(", ")
							+ "</h5>"
						);
						el.prepend("<hr>");
					}
				}
				_class.rendered = true;
				
				if (!_class.subclasses) return;
				_class.subclasses.map (function (subclass){
					renderClassProperties(subclass);
				});
			})(document.classTree.classes[class_uri]);
		});

		// Render rest document properties
		var counter=0;
		var el = $("<div>");
		
		Object.getOwnPropertyNames(properties).map ( function (property_uri) {
			if (property_uri == "@") return;
			if (properties[property_uri] == "rendered") return;
			try {
				renderDocumentProperty(veda, document, property_uri, document_single_property_template, el);
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
