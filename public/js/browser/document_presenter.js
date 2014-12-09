// Document Presenter

veda.Present(function Document(veda) { "use strict";
	
	var cnt = 0;
	
	veda.on("document:loaded", function (document, container_param, template_param, _mode) {
		
		console.log("document presenter:", ++cnt, document.id, document);
		
		var container = container_param || $("#main");
		
		var mode = _mode || "view";
		
		container.empty().hide();
		
		// Embedded templates list
		var embedded = [];
				
		var templates;
		
		// Extracted scripts
		var scripts = [];

		var templateStr;
		
		if (template_param) {
			
			templateStr = template_param["v-ui:template"][0].toString();
			templateStr = templateStr.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
				scripts.push(script);
				return "";
			});
			templates = [ $( templateStr ) ];
			
		} else if ( document["rdf:type"] && document["rdf:type"].length) {
			templates = document["rdf:type"]
				.filter( function (item) {
					return item instanceof veda.IndividualModel;
				})
				.map( function (item) { 
					var _class = new veda.ClassModel(item);
					if (_class.documentTemplate["v-ui:template"]) {
						// Get template from class

						templateStr = _class.documentTemplate["v-ui:template"][0].toString()
						templateStr = templateStr.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
							scripts.push(script);
							return "";
						});
						return $( templateStr );
						
					}
					// Construct generic template
					return genericTemplate(document, _class); 
				})
		} else {
			templates = [ genericTemplate(document) ];
		}
		
		/*console.log("scripts:", scripts);
		console.log("templates:");
		templates.map( function (item) { console.log(item.html()) });*/

		templates.map( function (classTemplate) {
			
			// Actions
			var $edit = $("#edit", classTemplate),
				$save = $("#save", classTemplate),
				$cancel = $("#cancel", classTemplate);
			
			$edit
				.on("click", function (e) {
					document.trigger("edit");
				});
			
			$save.hide()
				.on("click", function (e) {
					document.trigger("save");
				});
			
			$cancel.hide()
				.on("click", function (e) {
					document.trigger("cancel");
				});
			
			// Define handlers
			document.on("edit", function () {
				$edit.hide();
				$save.show();
				$cancel.show();
			});
			
			document.on("save", function () {
				document.save();
				document.trigger("view");
				$save.hide();
				$cancel.hide();
				$edit.show();
				// Change location.hash if document was presented in #main container
				if (!container_param) riot.route("#/document/" + document.id, false);
			});
			
			document.on("cancel", function () {
				// Clear defined handlers
				document.off("view edit save cancel");
				document.reset();
			});
			
			// Trigger same events for embedded templates
			document.on("view edit save", function (event) {
				embedded.map(function (item) {
					item.trigger(event);
				});
			});
			
			// About
			$("[about]", classTemplate).map( function () {
				
				var propertyContainer = $( this ), 
					about = new veda.IndividualModel(propertyContainer.attr("about")),
					property_uri = propertyContainer.attr("property");
				if (property_uri == "id") propertyContainer.html( about[property_uri] );
				else propertyContainer.html( about[property_uri].join(", ") );
				
			});
			
			// Object links				
			$("[rel]", classTemplate).map( function () {
				
				var relContainer = $(this), 
					rel_uri = relContainer.attr("rel"),
					relTemplate = relContainer.attr("template");
				
				relTemplate = relTemplate ? (
					new veda.IndividualModel(relTemplate) 
				) : (
					!document[rel_uri] || !document[rel_uri][0] || !document[rel_uri][0]["rdfs:label"] ? 
						new veda.IndividualModel("mnd-d:ClassNameIdTemplate") 
						: 
						new veda.IndividualModel("mnd-d:ClassNameLabelTemplate")
				)
				renderLink(document, rel_uri, relContainer, relTemplate, mode, embedded);
				
			});
			
			// Properties
			$("[property]", classTemplate).not("[about]").map( function () {
				
				var propertyContainer = $(this), 
					property_uri = propertyContainer.attr("property"),
					propertyTemplate = propertyContainer.attr("template");
					
				renderProperty(document, property_uri, propertyContainer, mode);
				
			});
			
			// Specials
			$("[href='id']", classTemplate).map( function () {
				$( this ).attr("href", "#/document/" + document.id);
			});
			
			container.append(classTemplate);
			
			document.trigger(mode);
			
			container.fadeIn(250);

			scripts.map( function (item) { 
				eval(item); 
			});
			
		});
	});
	
	
	function renderLink (document, rel_uri, relContainer, relTemplate, mode, embedded) {
		
		relContainer.empty().hide();
		
		if ( !document[rel_uri] ) document.defineProperty(rel_uri);
		
		if (document[rel_uri].length) {
			document[rel_uri].map( function (value) {renderValue (value, mode)} );
		}
		
		var template = $( $("#link-control-template").html() );
		if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) {
			$(".add", template).on("click", function () {
				var lnk = renderValue(undefined, "edit"); 
			});
		} else $(".add", template).hide();
		
		if (mode == "view") template.hide();
		relContainer.after(template);
		
		document.on("edit", function () {
			template.show();
		});
		document.on("view", function () {
			template.hide();
		});
		
		$(".typeahead", template).on("keypress", function (e) {
			var input = $( this );
			var pos = input.position();
			var t = $("<div>").attr("style", "position:absolute; height:100px; width:" + input.width() + ";left:" + pos.left + "px;top:" + input.outerHeight() + "px; border:1px solid red" ).html("blah!");
			input.after(t);
		});
		
		// Search modal
		$(".search", template).on("click", function (e) {
			var $modal = $("#search-modal");
			var search = new veda.SearchModel(undefined, $(".modal-body", $modal) );
			$modal.modal();
			// Add found values
			$("button#ok", $modal).on("click", function (e) {
				$(this).off("click");
				var selected = [];
				for (var uri in search.selected) {
					selected.push( search.selected[uri] );
				}
				document[rel_uri] = document[rel_uri].concat(selected);
				selected.map( function (value) {renderValue (value, "edit")} );
			});
		});
		
		function renderValue(value, mode) {
			var clone = relContainer.clone();
			var lnk;
			if (value instanceof veda.IndividualModel || !value) {
				setTimeout( function () {
					if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) {
						lnk = new veda.DocumentModel(value, clone, relTemplate, mode);
						embedded.push(lnk);
						// New instance
						if (!value) document[rel_uri] = document[rel_uri].concat(lnk);
					} else {
						lnk = new veda.DocumentModel(value, clone, relTemplate);
					}
					
					clone.attr("style", "position:relative;");
					var clear = $( $("#link-clear-button-template").html() );
					
					if (mode == "view") { 
						clear.hide();
					} else {
						clear.hide();
						clone.on("mouseenter", function () { clear.show(); clone.toggleClass("bg-danger"); });
						clone.on("mouseleave", function () { clear.hide(); clone.toggleClass("bg-danger"); });
					}
					
					clone.append(clear);
					clear.on("click", function () {
						clone.fadeOut(250, function () { clone.remove() });
						document[rel_uri] = document[rel_uri].filter(function (item) { return item != lnk });
						if (embedded.length) embedded = embedded.filter(function (item) { return item != lnk });
					});
					document.on("edit", function () {
						clone.on("mouseenter", function () { clear.show(); clone.toggleClass("bg-danger"); });
						clone.on("mouseleave", function () { clear.hide(); clone.toggleClass("bg-danger"); });
					});
					document.on("view", function () {
						clear.hide();
						clone.off();
					});
					
				}, 0);
			} else {
				// External resources
				clone.append( $("<a>", {href: value, text: value}) );
			}
			relContainer.before(clone.show());
			return lnk;
		}
		
	}

	function renderProperty (document, property_uri, container, mode) {
		
		if ( property_uri != 'id' && !veda.dictionary[property_uri] ) return;
		
		container.empty();
		
		if (property_uri == "id") { 
			container.val(document[property_uri]).html(document[property_uri]); 
			return;
		}
		
		var property = veda.dictionary[property_uri],
			template, emptyValue;
		
		if ( !document[property_uri] ) document.defineProperty(property_uri);
		var values = document[property_uri];
		
		var range = property["rdfs:range"][0].id;
		var controlType, emptyVal;
		range == "xsd:boolean"  			? 	(controlType = $.fn.vedaBoolean,  emptyVal = new Boolean(false) ) :
		range == "xsd:integer"  			? 	(controlType = $.fn.vedaInteger,  emptyVal = undefined ) :
		range == "xsd:nonNegativeInteger"   ? 	(controlType = $.fn.vedaInteger,  emptyVal = undefined ) :
		range == "xsd:decimal"  			? 	(controlType = $.fn.vedaDecimal,  emptyVal = undefined ) :
		range == "xsd:dateTime" 			? 	(controlType = $.fn.vedaDatetime, emptyVal = undefined ) :
												(controlType = $.fn.vedaString,   emptyVal = new String("") ) ;
		
		if (!values.length) values.push( emptyVal );
		var controls = values.map( renderControl );
		
		controls.map(function (item) {
			item.trigger(mode);
		});
		
		document.on("view edit", function (mode) {
			controls
				.filter(function (item) {
					return !!item;
				})
				.map(function (item) {
					item.trigger(mode);
				});
		});
		
		function renderControl (value, index) {
			var opts = {};
			opts.value = value;
			opts.change = function (value) {
				values[index] = value;
				document[property_uri] = values;
			};
			opts.add = function () {
				values.push( emptyVal );
				var control = renderControl(emptyVal, values.length-1);
				controls.push(control);
				control.trigger("edit");
			};
			opts.remove = function () {
				values[index] = undefined;
				controls[index] = undefined;
				document[property_uri] = values;
			};
			var control = controlType.call( $("<span>"), opts );
			container.append(control);
			return control;
		}
		
	}
	
	function genericTemplate (document, _class) {
		// Construct generic template
		var template = $("<div/>").append( $("#generic-class-template").html() );
		var properties;

		if (_class) {
			properties = _class.domainProperties;
			$(".className", template).append (
				$("<span/>", {"about": _class.id, "property": "rdfs:label"})
			);
		} else {
			properties = document.properties;
			if (!properties["rdf:type"]) {
				$(".properties", template).append (
					$("<div/>").append( 
						$("<strong/>", {"about": "rdf:type", "property": "rdfs:label"}).addClass("text-muted"),
						$("<div/>", {"rel": "rdf:type"}),
						$("<hr/>").attr("style", "margin: 10px 0px")
					)
				);
			}
		}
		
		$(".properties", template).append (
			Object.getOwnPropertyNames(properties).map( function (property_uri, index, array) {
				var property = veda.dictionary[property_uri];
				if (property_uri == "rdfs:label") return;
				if (property_uri == "rdfs:class") return;
				
				var result = $("<div/>").append( 
					$("<strong/>", {"about": property_uri, "property": "rdfs:label"}).addClass("text-muted")
				);
				
				switch( property["rdfs:range"] ? property["rdfs:range"][0].id : "rdfs:Literal" ) {
					case "rdfs:Literal" : 
					case "xsd:string" : 
					case "xsd:boolean" : 
					case "xsd:nonNegativeInteger" : 
					case "xsd:integer" : 
					case "xsd:decimal" : 
					case "xsd:dateTime" :
						result.append( $("<div/>", {"property": property_uri}) ); 
					break
					default:
						result.append( $("<div/>", {"rel": property_uri}) ); 
					break
				}
				
				if (index < array.length-1) result.append( $("<hr/>").attr("style", "margin: 10px 0px") );
				
				return result;
				
			})
		);
		return template;
	}
	
});
