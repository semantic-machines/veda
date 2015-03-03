// Document Presenter

veda.Module(function DocumentPresenter(veda) { "use strict";
	
	//var c1 = 0;
	var deletedAlertTmpl = $("#deleted-document-alert-template").html();
	
	veda.on("document:loaded", function PresentDocument(document, container_param, template_param, _mode) {
		
		//console.log("document presenter:", ++c1, document.id, document);
		
		var container = container_param || $("#main");
		
		var mode = _mode || "view";
		
		container
			.empty().hide()
			.attr("resource", document.id)
			.attr("typeof", document["rdf:type"].map(function (item) { return item.id }).join(" ") );
			
		if (document["v-s:deleted"] && document["v-s:deleted"][0] == true) {
			var deletedAlert = $( deletedAlertTmpl );
			container.prepend(deletedAlert);
			$("button", deletedAlert).click(function () {
				document.trigger("recover");
			});
		}

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
					var _class = veda.ontology.classes[item.id];
					if (_class.documentTemplate && _class.documentTemplate["v-ui:template"]) {
						// If _class.documentTemplate is embedded => construct generic template
						if (_class.documentTemplate["v-ui:embedded"] && _class.documentTemplate["v-ui:embedded"][0] == true) {
							return genericTemplate(document, _class); 
						}
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
		
		// Cleanup memory
		templates.map(function (template) {
			template.on("remove", function (event) {
				document.trigger("document:cleanup");
				container = mode = document = container_param = template_param = _mode = null;
			});
		});
		
		// Trigger same events for embedded templates
		document.on("view edit save delete recover", function (event) {
			embedded.map(function (item) {
				item.trigger(event);
			});
		});

		// Define handlers
		document.on("save", function () {
			document.save();
			document.trigger("view");
		});
		
		document.on("cancel", function () {
			embedded.map(function (item) {
				item.reset();
			});
			document.cancel();
		});

		document.on("delete", function () {
			document.delete();
			document.trigger("cancel");
		});

		document.on("recover", function () {
			document.recover();
			document.trigger("cancel");
		});
		
		document.on("view edit", function (_mode) {
			mode = _mode;
		});

		templates.map( function (classTemplate) {

			// Actions
			var $edit = $("#edit", classTemplate),
				$save = $("#save", classTemplate),
				$cancel = $("#cancel", classTemplate),
				$delete = $("#delete", classTemplate);

			$delete.on("click", function (e) {
				if ( confirm("Вы действительно хотите удалить документ?") ) document.trigger("delete");
			});
			if (document["v-s:deleted"][0] && document["v-s:deleted"][0] == true) $delete.hide();

			$edit.on("click", function (e) {
				document.trigger("edit");
			});

			document.on("edit", function () {
				$edit.hide();
				$save.show();
				$cancel.show();					
			});
			
			$save.hide().on("click", function (e) {
				document.trigger("save");
			});
						
			document.on("save", function (e) {
				$save.hide();
				$cancel.hide();
				$edit.show();
				// Change location.hash if document was presented in #main container
				if (container.prop("id") === "main") riot.route("#/document/" + document.id, false);
			});
			
			$cancel.hide().on("click", function (e) {
				document.trigger("cancel");
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
						new veda.IndividualModel("v-ui:ClassNameIdTemplate") 
						: 
						new veda.IndividualModel("v-ui:ClassNameLabelTemplate")
				)
				renderLink(document, rel_uri, relContainer, relTemplate, mode, embedded);
				
				// Re-render link property if its' values were changed
				document.on("document:propertyModified", function (doc_property_uri) {
					if (doc_property_uri === rel_uri) {
						renderLink(document, rel_uri, relContainer, relTemplate, mode, embedded);
					}
				});
				
			});
			
			// Properties
			$("[property]", classTemplate).not("[about]").map( function () {
				
				var propertyContainer = $(this), 
					property_uri = propertyContainer.attr("property"),
					propertyTemplate = propertyContainer.attr("template");
					
				renderProperty(document, property_uri, propertyContainer, mode);
				
				// Re-render property if its' values were changed
				document.on("document:propertyModified", function (doc_property_uri) {
					if (doc_property_uri === property_uri) {
						renderProperty (document, property_uri, propertyContainer, mode);
					}
				});
				
			});
			
			// Specials
			$("[href='id']", classTemplate).map( function () {
				$( this )
					.attr("href", "#/document/" + document.id)
					.after( 
						$("<a>", {href: "#/graph/" + document.id}).append( 
							$("<i>").addClass("glyphicon glyphicon-link") 
						) 
					)
					.after( "&nbsp;" );
			});
			
			container.append(classTemplate);
			
			document.trigger(mode);
			
			container.show();
			
			scripts.map( function (item) { 
				var fun = new Function("veda", "document", item);
				fun(veda, document);
			});
			
		});
	});
	
	
	function renderLink (document, rel_uri, relContainer, relTemplate, mode, embedded) {
		
		relContainer.empty();
		
		if ( !document[rel_uri] ) document.defineProperty(rel_uri);
		
		if (document[rel_uri].length) {
			document[rel_uri].map( function (value) {renderValue (value, mode)} );
		}
		
		var opts = {
			limit: 20,
			queryPrefix: "('rdf:type'=='owl:Class'||'rdf:type'=='rdfs:Class')",
			select: function (selected) {
				document[rel_uri] = document[rel_uri].concat(selected);
			} 
		};
		if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) {
			opts.add = function () {
				document[rel_uri] = document[rel_uri].concat(new veda.IndividualModel());
			}
		}
		var control = $("<span>").vedaLink(opts);
		
		document.on("view edit", function (e) {
			if (e == "edit") {
				control.show();
			} else {
				control.hide();
			}
		});
		
		if (mode == "edit") {
			control.show();
		} else {
			control.hide();
		}
		
		relContainer.append(control);

		function renderValue(value, mode) {
			// Create the same tag container to preserve element layout
			var clone = $("<" + relContainer.prop("tagName") + ">");
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
						clone.remove();
						document[rel_uri] = document[rel_uri].filter(function (item) { return item.id != lnk.id });
						if (embedded.length) {
							var index = embedded.indexOf(lnk);
							if ( !(index<0) ) embedded.splice(index, 1);
						}
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
			relContainer.append(clone);
			return lnk;
		}
		
	}

	function renderProperty (document, property_uri, container, mode) {
		
		if ( property_uri != 'id' && !veda.ontology[property_uri] ) return;
		
		container.empty();
		
		if (property_uri == "id") { 
			container.val(document[property_uri]).html(document[property_uri]); 
			return;
		}
		
		var property = veda.ontology[property_uri],
			controlType, emptyVal;
		
		if ( !document[property_uri] ) document.defineProperty(property_uri);
		var values = document[property_uri];

		container.attr("content", values.join(", "));

		var range = property["rdfs:range"][0].id;
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
		
		document.on("view edit", function (_mode) {
			mode = _mode;
			controls
				.filter(function (item) {
					return !!item;
				})
				.map(function (item) {
					item.trigger(mode);
				});
		});
		
		function renderControl (value, index) {
			var opts = {
				value: value,
				change: function (value) {
					values[index] = value;
					document[property_uri] = values;
				},
				add: function () {
					values.push( emptyVal );
					document[property_uri] = values;
				},
				remove: function () {
					delete values[index];
					delete controls[index];
					document[property_uri] = values;
				}
			}
			var control = controlType.call( $("<span>"), opts );
			container.append(control);
			return control;
		}
		
	}
	
	function genericTemplate (document, _class) {
		// Construct generic template
		var propTmpl = $("#generic-property-template").html();
		var template = $("<div/>").append( $("#generic-class-template").html() );
		var properties;

		if (_class) {
			properties = _class.domainProperties;
			$(".className", template).append (
				$("<span/>", {"about": _class.id, "property": "rdfs:label"})
			);
		} else {
			properties = document.properties;
		}
		
		$(".properties", template).append (
			Object.getOwnPropertyNames(properties).map( function (property_uri, index, array) {
				var property = veda.ontology[property_uri];
				if (property_uri === "rdfs:label" || property_uri === "rdf:type" || property_uri === "v-s:deleted") return;
				
				var result = $("<div/>").append( propTmpl );
				$(".name", result).append (
					$("<strong/>", {"about": property_uri, "property": "rdfs:label"}).addClass("text-muted")
				)
				
				switch( property["rdfs:range"] ? property["rdfs:range"][0].id : "rdfs:Literal" ) {
					case "rdfs:Literal" : 
					case "xsd:string" : 
					case "xsd:boolean" : 
					case "xsd:nonNegativeInteger" : 
					case "xsd:integer" : 
					case "xsd:decimal" : 
					case "xsd:dateTime" :
						$(".value", result).append (
							$("<div/>", {"property": property_uri})
						);
					break
					default:
						$(".value", result).append (
							$("<div/>", {"rel": property_uri})
						);
					break
				}
				
				if (index < array.length-1) result.append( $("<hr/>").attr("style", "margin: 10px 0px") );
				
				return result;
				
			})
		);
		return template;
	}
	
});
