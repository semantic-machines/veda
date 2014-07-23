// Editor Presenter
Veda(function EditorPresenter(veda) { "use strict";

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
								(item["rdfs:label"] ? item["rdfs:label"].filter( function (item) { return item.language == veda.user.language || item.language == "NONE" }).join(", ") : item["@"]) + "</a>";
						else return item;
					})
					.filter(function (item) { return item.language ? item.language == veda.user.language || item.language == "NONE" : item })
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
	var editor_template = $("#editor-template").html();
	var editor_single_property_template = $("#editor-single-property-template").html();
	
	veda.on("editor:loaded", function (editor, container_param) {
		
		var container = container_param || $("#main");
		container.html(editor_template);
		localize(container, veda.user.language);
		
		// Render document properties (respecting classes)
		var renderedProperties = {};
		Object.getOwnPropertyNames(editor.properties).reduce( function (accumulator, property_uri) {
			accumulator[property_uri] = undefined;
			return accumulator;
		}, renderedProperties);

		var renderedClasses = {};
		Object.getOwnPropertyNames(editor.classTree.roots).map( function (class_uri) {
			// Separate function for recursive calls
			(function renderClassProperties (_class) {
				if (!renderedClasses[_class["@"]]) {
					var counter = 0;
					var el = $("<div>");
					Object.getOwnPropertyNames(_class.domainProperties).map( function (property_uri) {
						try {
							renderDocumentProperty(veda, editor, property_uri, editor_single_property_template, el);
							counter++;
							renderedProperties[property_uri] = "rendered";
						} catch (e) {
							return;
						}
					});
					if (counter) { 
						$("#editor-properties", container).append(el);
						el.prepend("<h5 class='text-muted text-right'>" 
							+ 
							_class["rdfs:label"]
								.filter( function (item) { return item.language ? item.language == veda.user.language || item.language == "NONE" : item } )
								.join(", ")
							+ "</h5>"
						);
						//el.prepend("<hr>");
					}
				}
				renderedClasses[_class["@"]] = "rendered";
				
				if (!_class.subClasses) return;
				_class.subClasses.map (function (subClass) {
					renderClassProperties(subClass);
				});
			})(editor.classTree.classes[class_uri]);
		});

		// Render rest document properties
		var counter = 0;
		var el = $("<div>");
		
		Object.getOwnPropertyNames(renderedProperties).map ( function (property_uri) {
			if (property_uri == "@") return;
			if (renderedProperties[property_uri] == "rendered") return;
			try {
				renderDocumentProperty(veda, editor, property_uri, editor_single_property_template, el);
				counter++;
			} catch (e) {
				return;
			}
		});
		if (counter) {
			$("#editor-properties", container).append(el);
			//el.prepend("<hr>");
		}
		
		$("#save", container).on("click", function () {
			editor.save();
		});
		
	});

});
