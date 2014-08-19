// Document Presenter

Veda(function DocumentPresenter2(veda) { "use strict";

	function renderDocumentProperty (veda, individual, property_uri, template, container) {
		var label, uri, values;
		label = typeof individual.properties[property_uri] == "object" ? 
					individual.properties[property_uri]["rdfs:label"]
						.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
						.join(", ")
					: property_uri;
		uri = typeof individual.properties[property_uri] == "object" ? "#/document/" + individual.properties[property_uri].id : "";
		values = individual[property_uri]
					.map( function (item) {
						if (item instanceof String)
							// Check if string starts with http:// or ftp://
							return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
						else if (item instanceof IndividualModel)
							return "<a data-toggle='popover' href='#/document/" + item.id + "'>" + 
								(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item.id) + "</a>";
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
	
	veda.on("document2:loaded", function (document, container_param) {
		
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
						.join(", ") : document.id,
					uri: "#/document2/" + document.id
				}
			) 
		);

		// Render document properties
		Object.getOwnPropertyNames(document.properties).map ( function (property_uri) {
			try {
				renderDocumentProperty (veda, document, property_uri, document_single_property_template, $("#document-properties", container));
			} catch (e) {}
		});
		
	});

	function renderProperty (property, spec, values) {
		
		var propertyTemplate = $("#property-template").html();
		var renderedProperty = "";
		var renderedValues = "";
		
		switch( property["rdfs:range"][0].id ) {

			case "rdfs:Literal" : 
			case "xsd:string" : 
				var valueTemplate = $("#string-value-template").html();
				
				values
					.filter (function (value) {
						return value.language == Veda().user.language;
					})
					.map (function (value) {
						renderedValues += riot.render(valueTemplate, {value: value});
					});
	
				break

			case "xsd:boolean" : 
			case "xsd:integer" : 
			case "xsd:decimal" : 
			case "xsd:dateTime" : 
				var valueTemplate = $("#datetime-value-template").html();
				
				values
					.map (function (value) {
						renderedValues += riot.render(valueTemplate, {value: value});
					});

				break

			default : 
				break
		}

		renderedProperty = riot.render (
			propertyTemplate, 
			{property: property, values: renderedValues},
			function (value) {
				if ( !(value instanceof Array) ) return value;
				var res = value.filter(function (item) {
					return !(item instanceof String) ? true : item.language == Veda().user.language;
				});
				return res;
			}
		);
		return renderedProperty;
	
	}
	
	veda.on("document2:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		
		//container.empty();
		
		document["rdf:type"]
			.filter( function (item) {
				return item instanceof IndividualModel
			})
			.map( function (item) {
				
				var _class = new ClassModel(veda, item);
				var template = _class.documentTemplate["veda-ui:template"] ? _class.documentTemplate["veda-ui:template"][0] : undefined;
				var renderedProperties = {};
				
				Object.getOwnPropertyNames(document.properties).reduce (function (acc, property_uri) {

					var property = document.properties[property_uri];
					var spec = _class.specsByProps[property_uri];
					var values = document[property_uri];
					acc[property_uri] = renderProperty(property, spec, values);
					return acc;
					
				}, renderedProperties);
				
				renderedProperties.id = document.id;
				
				/*var renderedTemplate = riot.render (
					template, 
					{
						document: renderedProperties, 
						_class: _class
					}, 
					function (value) {
						if ( !(value instanceof Array) ) return value;
						var res = value.filter(function (item) {
							return !(item instanceof String) ? true : item.language == Veda().user.language || item.language == "NONE";
						});
						return res;
					}
				);
				
				var renderedDocument = $("<div/>");
				renderedDocument.append(renderedTemplate);
				
				container.append(renderedDocument); */
				
				var renderedDocument = $("<div/>");
				renderedDocument.append(template);
				
				var data = {
					document: renderedProperties, 
					_class: _class
				};
				
				$('[data-property]', renderedDocument).each( function () {
					var $this = $(this);
					var key = $this.data("property").split(".").reduce(function (acc, i) {
							return isNaN(i) ? acc + "['" + i + "']" : acc + "[" + i + "]";
					}, "");
					$this.empty().append( eval("data"+key) );
				});
				
				container.append( renderedDocument );
				
			});
		
		var actionsTemplate = $("#actions").html();
		container.append(actionsTemplate);
		
		$("#edit", container).on("click", function (e) {
			$(".value, .input-control, .value-control").toggleClass("hidden");
			
			$(this).toggleClass("hidden");
			$("#save", container).toggleClass("hidden");
			$("#cancel", container).toggleClass("hidden");
		});

		$("#save", container).on("click", function (e) {
			document.save();
			$(".value, .input-control, .value-control").toggleClass("hidden");
			
			$(this).toggleClass("hidden");
			$("#edit", container).toggleClass("hidden");
			$("#cancel", container).toggleClass("hidden");			
		});

		$("#cancel", container).on("click", function (e) {
			$(".value, .input-control, .value-control").toggleClass("hidden");
			
			$(this).toggleClass("hidden");
			$("#edit", container).toggleClass("hidden");
			$("#save", container).toggleClass("hidden");
		});

	});

});
