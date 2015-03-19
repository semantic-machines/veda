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

		var classTemplate, templateStr, specs = {}, scripts = [];
		
		if (template_param) {
			templateStr = template_param["v-ui:template"][0].toString();
			templateStr = templateStr.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
				scripts.push(script);
				return "";
			});
			classTemplate = $( templateStr );
			specs = $.extend.apply (this, [specs].concat(
				document["rdf:type"]
					.filter( function (_class) {
						return _class instanceof veda.IndividualModel;
					})
					.map( function (_class) {
						return _class.specsByProps;
					})
				)
			);
			renderTemplate (document, container, classTemplate, specs, scripts, mode);

		} else if (
			!document["rdf:type"]
				.filter( function (_class) {
					return _class instanceof veda.IndividualModel;
				})
				.map( function (_class) {
					if (_class.documentTemplate && _class.documentTemplate["v-ui:template"]) {
						// If _class.documentTemplate is embedded => construct generic template
						if (_class.documentTemplate["v-ui:embedded"] && _class.documentTemplate["v-ui:embedded"][0] == true) {
							classTemplate = genericTemplate(document, _class); 
						} else {
							// Get template from class
							templateStr = _class.documentTemplate["v-ui:template"][0].toString()
							templateStr = templateStr.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
								scripts.push(script);
								return "";
							});
							classTemplate = $( templateStr );
						}
					} else {
						// Construct generic template
						classTemplate = genericTemplate(document, _class);
					}
					specs = _class.specsByProps || {};
					renderTemplate (document, container, classTemplate, specs, scripts, mode);
				})
				.length
		) {
			classTemplate = genericTemplate(document);
			renderTemplate (document, container, classTemplate, specs, scripts, mode);
		}
	});
	
	
	function valueValidator (spec) {
		var validator = function (value) {
			var result = true;
			
			if (!spec) return result;
			
			// regexp check
			if (spec.hasValue("v-ui:regexp")) { 
				var regexp = new RegExp(spec["v-ui:regexp"][0]);
				result = result && regexp.test(value.toString());
			}
			
			// range check
			switch (spec["rdf:type"][0].id) {
				case "v-ui:PropertySpecification" :
				case "v-ui:IntegerPropertySpecification" :
					if (spec.hasValue("v-ui:minIntegerValue")) result = result && (value >= spec["v-ui:minIntegerValue"][0]);
					if (spec.hasValue("v-ui:maxIntegerValue")) result = result && (value <= spec["v-ui:maxIntegerValue"][0]);
					break
				case "v-ui:DecimalPropertySpecification" :
					if (spec.hasValue("v-ui:minDecimalValue")) result = result && (value >= spec["v-ui:minDecimalValue"][0]);
					if (spec.hasValue("v-ui:maxDecimalValue")) result = result && (value <= spec["v-ui:maxDecimalValue"][0]);
					break
				case "v-ui:DatetimePropertySpecification" :
					if (spec.hasValue("v-ui:minDatetimeValue")) result = result && (value >= spec["v-ui:minDatetimeValue"][0]);
					if (spec.hasValue("v-ui:maxDatetimeValue")) result = result && (value <= spec["v-ui:maxDatetimeValue"][0]);
					break
				case "v-ui:StringPropertySpecification" :
					if (spec.hasValue("v-ui:minLength")) result = result && (value.length >= spec["v-ui:minLength"][0]);
					if (spec.hasValue("v-ui:maxLength")) result = result && (value.length <= spec["v-ui:maxLength"][0]);
					break
				case "v-ui:BooleanPropertySpecification" :
				case "v-ui:ObjectPropertySpecification" :
			}
			return result;
		}
		return validator;
	}
	
	function renderTemplate (document, container, classTemplate, specs, scripts, mode) {
		
		// Embedded templates list
		var embedded = [];
				
		// Trigger same events for embedded templates
		document.on("view edit save delete recover", function (event) {
			embedded.map(function (item) {
				if (item.trigger) item.trigger(event);
			});
		});

		// Define handlers
		document.on("save", function () {
			document.save();
			document.trigger("view");
		});
		
		document.on("cancel", function () {
			embedded.map(function (item) {
				if (item.reset) item.reset();
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
		
		// Cleanup memory
		classTemplate.on("remove", function (event) {
			document.trigger("document:cleanup");
			$(".typeahead", container).typeahead("destroy");
			container = mode = document = container_param = template_param = _mode = null;
		});
		
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
			
			if (specs[about.id] && specs[about.id]["v-ui:minCardinality"] && specs[about.id]["v-ui:minCardinality"][0] > 0)
				propertyContainer.append( $("<span class='text-danger'> *</span>") );
			
		});
		
		// Object links				
		$("[rel]", classTemplate).map( function () {
			
			var relContainer = $(this), 
				rel_uri = relContainer.attr("rel"),
				relTemplate = relContainer.attr("template"),
				spec = specs[rel_uri];
			
			relTemplate = relTemplate ? (
				new veda.IndividualModel(relTemplate) 
			) : (
				!document[rel_uri] || !document[rel_uri][0] || !document[rel_uri][0]["rdfs:label"] ? 
					new veda.IndividualModel("v-ui:ClassNameIdTemplate") 
					: 
					new veda.IndividualModel("v-ui:ClassNameLabelTemplate")
			)
			renderLink(document, rel_uri, relContainer, relTemplate, spec, mode, embedded);
			
			// Re-render link property if its' values were changed
			document.on("document:propertyModified", function (doc_property_uri) {
				if (doc_property_uri === rel_uri) {
					renderLink(document, rel_uri, relContainer, relTemplate, spec, mode, embedded);
				}
			});
			
		});
		
		// Properties
		$("[property]", classTemplate).not("[about]").map( function () {
			
			var propertyContainer = $(this), 
				property_uri = propertyContainer.attr("property"),
				propertyTemplate = propertyContainer.attr("template"),
				spec = specs[property_uri];
				
			renderProperty(document, property_uri, propertyContainer, spec, mode);
			
			// Re-render property if its' values were changed
			document.on("document:propertyModified", function (doc_property_uri) {
				if (doc_property_uri === property_uri) {
					renderProperty (document, property_uri, propertyContainer, spec, mode);
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
	}
	
	
	function renderLink (document, rel_uri, relContainer, relTemplate, spec, mode, embedded) {
		
		relContainer.empty();
		
		if ( !document[rel_uri] ) document.defineProperty(rel_uri);
		
		if (document[rel_uri].length) {
			document[rel_uri].map( function (value) {renderValue (value, mode)} );
		}
		
		var opts = {
			limit: 30,
			//queryPrefix: "('rdf:type'=='owl:Class')",
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

	function renderProperty (document, property_uri, container, spec, mode) {
		
		if ( property_uri != 'id' && !veda.ontology[property_uri] ) return;
		
		container.empty();
		
		if (property_uri == "id") { 
			container.val(document[property_uri]).html(document[property_uri]); 
			return;
		}
		
		var property = veda.ontology[property_uri],
			controlType, emptyVal, defaultValue;
		
		if ( !document[property_uri] ) document.defineProperty(property_uri);
		var values = document[property_uri];

		container.attr("content", values.join(", "));

		switch (property["rdfs:range"][0].id) {
			case "xsd:boolean": 
				controlType = $.fn.vedaBoolean; 
				emptyVal = spec && spec["v-ui:defaultBooleanValue"] && spec["v-ui:defaultBooleanValue"][0] ? spec["v-ui:defaultBooleanValue"][0] : new Boolean(false);
				break
			case "xsd:integer": 
			case "xsd:nonNegativeInteger":
				controlType = $.fn.vedaInteger; 
				emptyVal = spec && spec["v-ui:defaultIntegerValue"] && spec["v-ui:defaultIntegerValue"][0] ? spec["v-ui:defaultIntegerValue"][0] : undefined; 
				break
			case "xsd:decimal":
				controlType = $.fn.vedaDecimal; 
				emptyVal = spec && spec["v-ui:defaultDecimalValue"] && spec["v-ui:defaultDecimalValue"][0] ? spec["v-ui:defaultDecimalValue"][0] : undefined; 
				break
			case "xsd:dateTime": 
				controlType = $.fn.vedaDatetime; 
				emptyVal = spec && spec["v-ui:defaultDatetimeValue"] && spec["v-ui:defaultDatetimeValue"][0] ? spec["v-ui:defaultDatetimeValue"][0] : undefined; 
				break
			default: 
				controlType = $.fn.vedaString; 
				emptyVal = spec && spec["v-ui:defaultStringValue"] && spec["v-ui:defaultStringValue"][0] ? spec["v-ui:defaultStringValue"][0] : new String(""); 
				break
		}
		
		var validator = valueValidator(spec);
		
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
					if (validator(value)) {
						control.addClass("has-success");
						values[index] = value;
						document[property_uri] = values;
					} else {
						control.addClass("has-error");
					}
				},
				add: function () {
					values.push( emptyVal );
					document[property_uri] = values;
				},
				remove: function () {
					values.splice(index, 1);
					controls.splice(index, 1);
					document[property_uri] = values;
				}
			}
			var control = controlType.call( $("<span>"), opts );
			
			validator(value) ? control.addClass("has-success") : control.addClass("has-error");
			
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
