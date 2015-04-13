// Document Presenter

veda.Module(function DocumentPresenter(veda) { "use strict";
	
	//var c1 = 0;
	
	var deletedAlertTmpl = $("#deleted-document-alert-template").html();
	
	veda.on("document:loaded", function PresentDocument(document, container_param, template_param, _mode) {
		
		//console.log("document presenter:", ++c1, document.id, document);
		
		var container = container_param || $("#main");
		
		var mode = _mode || "view";
		
		container
			.empty()//.hide()
			.attr("resource", document.id)
			.attr("typeof", document["rdf:type"].map(function (item) { return item.id }).join(" ") );

		// Change location.hash if document was presented in #main container
		/*if (container.prop("id") === "main") {
			var hash = ["#/document", document.id, container_param || "", template_param || "", mode || ""].join("/");
			riot.route(hash, false);
		}*/
			
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
					if (_class.template && _class.template["v-ui:template"]) {
						// If _class.template is embedded => construct generic template
						if (_class.template["v-ui:embedded"] && _class.template["v-ui:embedded"][0] == true) {
							classTemplate = genericTemplate(document, _class); 
						} else {
							// Get template from class
							templateStr = _class.template["v-ui:template"][0].toString()
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
			// Change location.hash if document was presented in #main container
			if (container.prop("id") === "main") riot.route("#/document/" + document.id, false);
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
		
		document.on("view edit search", function (_mode) {
			mode = _mode;
		});
		
		// Cleanup memory
		classTemplate.on("remove", function (event) {
			document.trigger("document:cleanup");
			$(".typeahead", container).typeahead("destroy");
			container = mode = document = container_param = template_param = _mode = null;
		});
		
		// Actions
		var $edit = $("#edit.action", classTemplate),
			$save = $("#save.action", classTemplate),
			$cancel = $("#cancel.action", classTemplate),
			$delete = $("#delete.action", classTemplate),
			$search = $("#search.action", classTemplate);

		// Show / hide buttons in different modes
		document.on("view edit search", function (mode) {
			mode === "view"   ? ( $edit.show(), $save.hide(), $cancel.hide(), $delete.show(), $search.hide() ) :
			mode === "edit"   ? ( $edit.hide(), $save.show(), $cancel.show(), $delete.show(), $search.hide() ) :
			mode === "search" ? ( $edit.hide(), $save.hide(), $cancel.hide(), $delete.hide(), $search.show() ) : 
			true;
		});

		// Buttons handlers
		// Edit
		$edit.on("click", function (e) {
			document.trigger("edit");
		});
		
		// Save
		$save.on("click", function (e) {
			document.trigger("save");
		});
		document.on("validation:complete", function (e) {
			var isValid = Object.keys(document.isValid).reduce(function (state, specId) {
				return state && document.isValid[specId];
			}, true);
			isValid ? $save.removeAttr("disabled") : $save.attr("disabled", "disabled");
		});
		
		//  Cancel
		$cancel.on("click", function (e) {
			document.trigger("cancel");
		});
		
		//  Delete
		$delete.on("click", function (e) {
			if ( confirm("Вы действительно хотите удалить документ?") ) document.trigger("delete");
		});
		if (document["v-s:deleted"][0] && document["v-s:deleted"][0] == true) $delete.hide();
		
		// Search
		$search.on("click", function (e) {
			// serialize document as search query
			var query;
			var allProps = Object.getOwnPropertyNames(document.properties)
				.map(function (property_uri) {
					var property = document.properties[property_uri];
					var values = document[property_uri].filter(function(item){return !!item && !!item.valueOf()});
					var oneProp;
					switch (property["rdfs:range"][0].id) {
						case "xsd:integer": 
						case "xsd:nonNegativeInteger":
						case "xsd:decimal":
							oneProp =
								values.length === 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[0] + "]" :
								values.length > 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[values.length-1] + "]" :
								undefined;
							break
						case "xsd:dateTime": 
							oneProp =
								values.length === 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[0].toISOString().substring(0,19) + "]" :
								values.length > 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[values.length-1].toISOString().substring(0,19) + "]" :
								undefined;
							break
						case "xsd:boolean": 
						case "xsd:string": 
						case "rdfs:Literal": 
							oneProp = values
								.filter(function(item){return !!item && !!item.valueOf()})
								.map( function (value) {
									return "'" + property_uri + "'=='" + value + "'";
								})
								.join("||");
							break
						default:
							oneProp = values
								.filter( function (value) {
									return value instanceof veda.IndividualModel;
								})
								.map( function (value) {
									return "'" + property_uri + "'=='" + value.id + "'";
								})
								.join("||");
							break
					}
					return oneProp ? "(" + oneProp + ")" : undefined;
				})
				.filter(function(item){return !!item;})
				.join("&&");
			query = allProps ? "(" + allProps + ")" : undefined;
			
			// Open Search
			var search = new veda.SearchModel(query);
			// Place document to params tab in Search caontainer
			new veda.DocumentModel(document.id, $("#params-" + search.id, search.view), undefined, "search");
		});
		
		// Process RDFa compliant template
		// About resources
		$("[about]", classTemplate).map( function () {
			
			var propertyContainer = $( this ), 
				about = new veda.IndividualModel(propertyContainer.attr("about")),
				property_uri = propertyContainer.attr("property");
			if (property_uri == "id") propertyContainer.html( about[property_uri] );
			else propertyContainer.html( about[property_uri].join(", ") );
		});
		
		// Related resources
		$("[rel]", classTemplate).map( function () {
			
			var relContainer = $(this), 
				rel_uri = relContainer.attr("rel"),
				containerParent = relContainer.parent(),
				relTemplate = relContainer.attr("template"),
				spec = specs[rel_uri];
			
			relContainer.empty().hide();
			
			relTemplate = relTemplate ? (
				new veda.IndividualModel(relTemplate) 
			) : (
				document.hasValue(rel_uri) && document[rel_uri][0].hasValue("rdfs:label") ? 
					new veda.IndividualModel("v-ui:ClassNameLabelTemplate")
					:
					new veda.IndividualModel("v-ui:ClassNameIdTemplate")
			)
			var rendered = renderLink(document, rel_uri, relContainer, containerParent, relTemplate, spec, mode, embedded);
			
			// Re-render link property if its' values were changed
			document.on("document:propertyModified", function (doc_property_uri) {
				if (doc_property_uri === rel_uri) {
					rendered.map( function (item) { item.remove(); } );
					rendered = renderLink(document, rel_uri, relContainer, containerParent, relTemplate, spec, mode, embedded);
				}
			});
			
		});
		
		// Properties
		$("[property]", classTemplate).not("[about]").map( function () {
			
			var propertyContainer = $(this),
				property_uri = propertyContainer.attr("property"),
				//propertyTemplate = propertyContainer.attr("template"),
				spec = specs[property_uri];
				
			renderProperty(document, property_uri, propertyContainer, spec, mode);
			
			// Re-render property if its' values were changed
			document.on("document:propertyModified", function (doc_property_uri) {
				if (doc_property_uri === property_uri) {
					renderProperty (document, property_uri, propertyContainer, spec, mode);
				}
			});
			
		});
		
		// Specials (not RDFa)
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
		
		//container.show();
		
		scripts.map( function (item) { 
			var presenter = new Function("veda", "document", "container", item + "//# sourceURL=" + document["rdf:type"][0].id + "Presenter.js");
			presenter(veda, document, classTemplate);
		});
	}
	
	function renderLink (document, rel_uri, relContainer, containerParent, relTemplate, spec, mode, embedded) {
		
		if ( !document[rel_uri] ) document.defineProperty(rel_uri);
		
		var values = document[rel_uri];
		
		var renderedValues = document.hasValue(rel_uri) ? 
			document[rel_uri].map( function (value) { return renderValue (value, mode)} ) : [];

		var controlContainer;
		if (renderedValues.length) {
			controlContainer = renderedValues[0];
		} else {
			controlContainer = relContainer.clone();
			relContainer.after(controlContainer);
		}
		
		var opts = {
			limit: 100,
			select: function (selected) {
				document[rel_uri] = document[rel_uri].concat(selected);
			} 
		};
		if (spec && spec.hasValue("v-ui:queryPrefix")) {
			opts.queryPrefix = spec["v-ui:queryPrefix"][0];
		}
		if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) {
			opts.add = function () {
				var embedded = new veda.IndividualModel();
				if (relTemplate.hasValue("v-ui:forClass")) {
					embedded["rdf:type"] = [relTemplate["v-ui:forClass"][0]];
				}
				document[rel_uri] = document[rel_uri].concat(embedded);
			}
		}
		var control = $("<span>").vedaLink(opts);
		
		// tooltip from spec
		if (spec && spec.hasValue("v-ui:tooltip")) {
			control.tooltip({
				title: spec["v-ui:tooltip"].join(", "),
				placement: "auto bottom",
				container: control,
				trigger: "focus"
			});
		}
		
		document.on("view edit search", function (mode) {
			mode === "view" ? control.hide() : 
			mode === "edit" ? control.show() : 
			mode === "search" ? control.show() : 
			true;
		});
		
		if (mode !== "search") isValid(document, spec, values) ? control.addClass("has-success") : control.addClass("has-error") ;
		
		setTimeout( function () {
			controlContainer.append(control).show();
		}, 0);
		
		return [controlContainer].concat(renderedValues);

		function renderValue(value, mode) {
			// Create the same tag container to preserve element layout
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
					
					var clear;
					if (clone.children(":first").css("display") === "inline") {
						clear = $( $("#link-clear-inline-button-template").html() );
					} else {
						clear = $( $("#link-clear-block-button-template").html() );
					}
										
					mode === "view" ? clear.hide() : clear.show() ;
					document.on("view edit search", function (mode) {
						mode === "view" ? clear.hide() :
						mode === "edit" ? clear.show() :
						clear.show();
					});					
					
					clone.append(clear);
					clear.on("click", function () {
						clone.remove();
						document[rel_uri] = document[rel_uri].filter(function (item) { return item.id != lnk.id });
						if (embedded.length) {
							var index = embedded.indexOf(lnk);
							if ( !(index<0) ) embedded.splice(index, 1);
						}
					});
					
				}, 0);
			} else {
				// External resources
				clone.append( $("<a>", {href: value, text: value}) );
			}
			relContainer.after(clone);
			return clone.show();
		}
		
	}

	function renderProperty (document, property_uri, container, spec, mode) {
		
		if ( property_uri != "id" && !veda.ontology[property_uri] ) return;
		
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
				emptyVal = spec && spec.hasValue("v-ui:defaultBooleanValue") ? spec["v-ui:defaultBooleanValue"][0] : new Boolean(false);
				break
			case "xsd:integer": 
			case "xsd:nonNegativeInteger":
				controlType = $.fn.vedaInteger; 
				emptyVal = spec && spec.hasValue("v-ui:defaultIntegerValue") ? spec["v-ui:defaultIntegerValue"][0] : undefined; 
				break
			case "xsd:decimal":
				controlType = $.fn.vedaDecimal; 
				emptyVal = spec && spec.hasValue("v-ui:defaultDecimalValue") ? spec["v-ui:defaultDecimalValue"][0] : undefined; 
				break
			case "xsd:dateTime": 
				controlType = $.fn.vedaDatetime; 
				emptyVal = spec && spec.hasValue("v-ui:defaultDatetimeValue") ? spec["v-ui:defaultDatetimeValue"][0] : undefined; 
				break
			default: 
				controlType = $.fn.vedaString; 
				emptyVal = spec && spec.hasValue("v-ui:defaultStringValue") ? spec["v-ui:defaultStringValue"][0] : new String(); 
				break
		}
		
		var controls = values.map( renderControl );
		if (!controls.length) controls.push( renderControl(emptyVal, 0) );
		
		controls.map(function (control) {
			control.trigger(mode);
			if (spec && spec.hasValue("v-ui:tooltip")) {
				control.tooltip({
					title: spec["v-ui:tooltip"].join(", "),
					placement: "auto bottom",
					container: control,
					trigger: "focus"
				});
			}
		});
		
		if (mode !== "search") isValid(document, spec, values) ? controls.map( function(item) {item.addClass("has-success")} ) : controls.map( function(item) {item.addClass("has-error")} );
		
		document.on("view edit search", function (_mode) {
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
					values.splice(index, 1);
					controls.splice(index, 1);
					document[property_uri] = values;
				}
			}
			var control = controlType.call( $("<span>"), opts );
			
			container.append(control);
			return control;
		}
		
	}

	function isValid (document, spec, values) {
		var result = true;
		if (!spec) return result;			
		// cardinality check
		if (spec.hasValue("v-ui:minCardinality")) { 
			result = result && (
				values.length >= spec["v-ui:minCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item && !!item.valueOf()}).length
			);
		}
		if (spec.hasValue("v-ui:maxCardinality")) { 
			result = result && (
				values.length <= spec["v-ui:maxCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item && !!item.valueOf()}).length
			);
		}
		// check each value
		result = result && values.reduce(function (result, value) {
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
					break
			}
			return result;
		}, result);
		document.isValid = document.isValid || {};
		document.isValid[spec.id] = result;
		document.trigger("validation:complete");
		return result;
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
