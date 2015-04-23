/*


// Individual Presenter

veda.Module(function IndividualPresenter(veda) { "use strict";
	
	var deletedAlertTmpl = $("#deleted-individual-alert-template").html();
	
	veda.on("individual:loaded", function (individual, container, template, mode) {
		
		if (!container) return;
		container = $(container);

		container
			.empty()
			.attr("resource", individual.id)
			.attr("typeof", individual["rdf:type"].map(function (item) { return item.id; }).join(" ") );

		// Change location.hash if individual was presented in #main container
		if (container.prop("id") === "main") {
			var hash = ["#/individual", individual.id, "#main"].join("/");
			riot.route(hash, false);
		}

		var specs = $.extend.apply (this, [].concat(
			individual["rdf:type"]
				.filter( function (_class) {
					return _class instanceof veda.IndividualModel;
				})
				.map( function (_class) {
					return _class.specsByProps;
				})
			)
		);
		var rendered = [], scripts = [];
		
		if (template) {
			if ( template instanceof jQuery ) {
				var $scripts = $("script", template);
				scripts = $scripts.map(function () { return $(this).text(); });
				$scripts.remove();
			}
			if (template instanceof veda.IndividualModel) {
				template = template["v-ui:template"][0].toString();
				template.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
					scripts.push(script);
					return "";
				});
				template = $(template);
			}
			if (template instanceof String || typeof template === "string") {
				template.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
					scripts.push(script);
					return "";
				});
				template = $(template);
			}
			rendered.push({
				template: renderTemplate(individual, template, specs),
				scripts: scripts
			});

		} else {
			rendered = individual["rdf:type"]
				.filter( function (_class) {
					return _class instanceof veda.IndividualModel;
				})
				.map( function (_class) {
					var template, scripts = [], specs;
					if (_class.template && _class.template["v-ui:template"]) {
						// If _class.template is embedded => construct generic template
						if (_class.template["v-ui:embedded"] && _class.template["v-ui:embedded"][0] == true) {
							template = genericTemplate(individual, _class);
						} else {
							// Get template from class
							template = _class.template["v-ui:template"][0].toString();
							template = template.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
								scripts.push(script);
								return "";
							});
							template = $(template);
						}
					} else {
						// Construct generic template
						template = genericTemplate(individual, _class);
					}
					specs = _class.specsByProps || {};
					return {
						template: renderTemplate(individual, template, specs),
						scripts: scripts
					};
				});
		}
		
		if (!rendered.length) {
			template = genericTemplate(individual);
			rendered.push({
				template: renderTemplate(individual, template, specs),
				scripts: scripts
			});
		}
		
		rendered.map( function (view) {
			view.template.trigger(mode);
			container.append(view.template);
			view.scripts.map( function (script) { 
				var presenter = new Function("veda", "individual", "container", script + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js");
				presenter(veda, individual, view.template);
			});
		});
	}
	
	function renderTemplate (individual, template, specs) {

		// Cleanup memory
		template.on("remove", function (event) {
			$(".typeahead", template).typeahead("destroy");
			individual = embedded = template = specs = null;
		});
		
		// Embedded templates list
		var embedded = [];
				
		// Trigger same events for embedded templates
		function syncEmbedded (event) {
			embedded.map(function (item) {
				item.trigger(event);
			});
		}
		template.on("view edit search save cancel delete recover", syncEmbedded);

		// Define handlers
		function saveHandler () {
			individual.save();
			template.trigger("view");
			// Change location.hash if individual was presented in #main container
			if (container.prop("id") === "main") {
				var hash = ["#/individual", individual.id, "#main"].join("/");
				riot.route(hash, false);
			}
		}
		template.on("save", saveHandler);

		function cancelHandler () {
			individual.reset();
			veda.trigger("individual:loaded", individual, container);
		}
		template.on("cancel", cancelHandler);
		
		function deleteHandler () {
			individual.delete();
			template.trigger("cancel");
		}
		template.on("delete", deleteHandler);

		function recoverHandler () {
			individual.recover();
			template.trigger("cancel");
		}
		template.on("recover", recoverHandler);

		function typeChangeHandler () {
			veda.trigger("individual:loaded", individual, container);
		}
		individual.on("individual:typeChanged", typeChangeHandler);
		template.one("remove", function () {
			individual.off("individual:typeChanged", typeChangeHandler);
		});
		
		// Actions
		var $edit = $("#edit.action", template),
			$save = $("#save.action", template),
			$cancel = $("#cancel.action", template),
			$delete = $("#delete.action", template),
			$search = $("#search.action", template);

		// Check rights to manage buttons		
		// Update
		if ($edit.length   && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $edit.remove();
		if ($save.length   && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $save.remove();
		if ($cancel.length && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $cancel.remove();
		if ($delete.length && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $delete.remove();
		// Delete
		if ($delete.length && !(individual.rights.hasValue("v-s:canDelete") && individual.rights["v-s:canDelete"][0] == true) ) $delete.remove();

		// Show / hide buttons in different modes
		function modeHandler (mode) {
			mode === "view"   ? ( $edit.show(), $save.hide(), $cancel.hide(), $delete.show(), $search.hide() ) :
			mode === "edit"   ? ( $edit.hide(), $save.show(), $cancel.show(), $delete.show(), $search.hide() ) :
			mode === "search" ? ( $edit.hide(), $save.hide(), $cancel.hide(), $delete.hide(), $search.show() ) : 
			true;
		}
		template.on("view edit search", modeHandler);

		// Buttons handlers
		// Edit
		$edit.on("click", function (e) {
			template.trigger("edit");
		});
		
		// Save
		$save.on("click", function (e) {
			template.trigger("save");
		});

		function validationHandler () {
			var isValid = Object.keys(individual.isValid).reduce(function (state, specId) {
				return state && individual.isValid[specId];
			}, true);
			isValid ? $save.removeAttr("disabled") : $save.attr("disabled", "disabled");
		}
		template.on("validation:complete", validationHandler);
		
		//  Cancel
		$cancel.on("click", function (e) {
			template.trigger("cancel");
		});
		
		//  Delete
		$delete.on("click", function (e) {
			if ( confirm("Вы действительно хотите удалить документ?") ) individual.trigger("delete");
		});
		if (individual.hasValue("v-s:deleted") && individual["v-s:deleted"][0]) $delete.hide();
		
		// Search
		$search.on("click", function (e) {
			// serialize individual as search query
			var query;
			var allProps = Object.getOwnPropertyNames(individual.properties)
				.map(function (property_uri) {
					var property = individual.properties[property_uri];
					var values = individual[property_uri].filter(function(item){return !!item && !!item.valueOf();});
					var oneProp;
					switch (property["rdfs:range"][0].id) {
						case "xsd:integer": 
						case "xsd:nonNegativeInteger":
						case "xsd:decimal":
							oneProp =
								values.length === 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[0] + "]" :
								values.length > 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[values.length-1] + "]" :
								undefined;
							break;
						case "xsd:dateTime": 
							oneProp =
								values.length === 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[0].toISOString().substring(0,19) + "]" :
								values.length > 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[values.length-1].toISOString().substring(0,19) + "]" :
								undefined;
							break;
						case "xsd:boolean": 
						case "xsd:string": 
						case "rdfs:Literal": 
							oneProp = values
								.filter(function(item){return !!item && !!item.valueOf();})
								.map( function (value) {
									return "'" + property_uri + "'=='" + value + "'";
								})
								.join("||");
							break;
						default:
							oneProp = values
								.filter( function (value) {
									return value instanceof veda.IndividualModel;
								})
								.map( function (value) {
									return "'" + property_uri + "'=='" + value.id + "'";
								})
								.join("||");
							break;
					}
					return oneProp ? "(" + oneProp + ")" : undefined;
				})
				.filter(function(item){return !!item;})
				.join("&&");
			query = allProps ? "(" + allProps + ")" : undefined;
			
			var individual_uri = individual.id;
			// Open Search
			var search = new veda.SearchModel(query);
			// Place individual to params tab in Search container
			var params = new veda.IndividualModel(individual_uri, $("#params-" + search.id, search.view), undefined, "search");
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
				relTemplate = relContainer.attr("template"),
				spec = specs[rel_uri];
			
			relContainer.empty().hide();
			
			relTemplate = relTemplate ? (
				new veda.IndividualModel(relTemplate) 
			) : (
				individual.hasValue(rel_uri) && individual[rel_uri][0].hasValue("rdfs:label") ? 
					new veda.IndividualModel("v-ui:ClassNameLabelTemplate")
					:
					new veda.IndividualModel("v-ui:ClassNameIdTemplate")
			);
			var rendered = renderLink(individual, rel_uri, relContainer, relTemplate, spec, mode, embedded);
			
			// Re-render link property if its' values were changed
			function propModHandler (doc_property_uri) {
				if (doc_property_uri === rel_uri) {
					rendered.map( function (item) { item.remove(); } );
					rendered = renderLink(individual, rel_uri, relContainer, relTemplate, spec, mode, embedded);
				}
			}
			individual.on("individual:propertyModified", propModHandler);
			individual.one("cleanup", function () {
				individual.off("individual:propertyModified", propModHandler);
			});

		});
		
		// Properties
		$("[property]", classTemplate).not("[about]").map( function () {
			
			var propertyContainer = $(this),
				property_uri = propertyContainer.attr("property"),
				//propertyTemplate = propertyContainer.attr("template"),
				spec = specs[property_uri];
				
			renderProperty(individual, property_uri, propertyContainer, spec, mode);
			
			// Re-render property if its' values were changed
			function propModHandler (doc_property_uri) {
				if (doc_property_uri === property_uri) {
					renderProperty (individual, property_uri, propertyContainer, spec, mode);
				}
			}
			individual.on("individual:propertyModified", propModHandler);
			individual.one("cleanup", function () {
				individual.off("individual:propertyModified", propModHandler);
			});
			
		});
		
		// Specials (not RDFa)
		$("[href='id']", classTemplate).map( function () {
			$( this )
				.attr("href", "#/individual/" + individual.id)
				.after( 
					$("<a>", {href: "#/graph/" + individual.id}).append( 
						$("<i>").addClass("glyphicon glyphicon-link") 
					) 
				)
				.after( "&nbsp;" );
		});
		
		container.append(classTemplate);
		
		individual.trigger(mode);
		
		//container.show();
		
		scripts.map( function (item) { 
			var presenter = new Function("veda", "individual", "container", item + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js");
			presenter(veda, individual, classTemplate);
		});
	}
	
	function renderLink (individual, rel_uri, relContainer, relTemplate, spec, mode, embedded) {
		
		if ( !individual[rel_uri] ) individual.defineProperty(rel_uri);
		
		function insDefObjVal () {
			individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
		}
		if ( spec && spec.hasValue("v-ui:defaultObjectValue") && !individual.hasValue(rel_uri) ) {
			individual.on("edit", insDefObjVal);
			individual.one("cleanup", function () {
				individual.off("edit", insDefObjVal);
			});
		}
		
		var immutable = spec && spec.hasValue("v-ui:immutable") && spec["v-ui:immutable"][0] == true;

		var values = individual[rel_uri];
		
		var renderedValues = individual.hasValue(rel_uri) ? 
			individual[rel_uri].map( function (value) { return renderValue (value, mode);} ) : [];

		var controlContainer;
		if (renderedValues.length) {
			controlContainer = renderedValues[renderedValues.length-1];
		} else {
			controlContainer = relContainer.clone();
			relContainer.after(controlContainer);
		}
		
		var opts = {
			limit: 100,
			select: function (selected) {
				individual[rel_uri] = individual[rel_uri].concat(selected);
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
				individual[rel_uri] = individual[rel_uri].concat(embedded);
			};
		}
		var control = $("<span>").vedaLink(opts);

		if (mode === "view") control.hide();
		if (mode === "edit" && immutable) control.hide();
		
		// tooltip from spec
		if (spec && spec.hasValue("v-ui:tooltip")) {
			control.tooltip({
				title: spec["v-ui:tooltip"].join(", "),
				placement: "auto bottom",
				container: control,
				trigger: "focus"
			});
		}
		
		if (mode !== "search" && !immutable) isValid(individual, spec, values) ? control.addClass("has-success") : control.addClass("has-error") ;
		
		setTimeout( function () {
			controlContainer.append(control).show();
		}, 0);

		function modeHandler (mode) {
			mode === "view" ? control.hide() : 
			mode === "edit" ? control.show() : 
			mode === "search" ? control.show() : 
			true;
		}
		if (!immutable) {
			individual.on("edit", modeHandler);
			individual.one("cleanup", function () {
				individual.off("edit", modeHandler);
			});
		}
		individual.on("view search", modeHandler);
		individual.one("cleanup", function () {
			individual.off("view", modeHandler);
			individual.off("search", modeHandler);
		});
		
		return [controlContainer].concat(renderedValues);

		function renderValue(value, mode) {
			// Create the same tag container to preserve element layout
			var clone = relContainer.clone();
			var lnk;
			if (value instanceof veda.IndividualModel || !value) {
				setTimeout( function () {
					if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) {
						lnk = new veda.IndividualModel(value, clone, relTemplate, mode);
						embedded.push(lnk);
						// New instance
						if (!value) individual[rel_uri] = individual[rel_uri].concat(lnk);
					} else {
						lnk = new veda.IndividualModel(value, clone, relTemplate);
					}
					
					clone.attr("style", "position:relative;");
					var clear = $( $("#link-clear-button-template").html() );
					
					if (clone.children(":first").css("display") === "inline") {
						clear = $( $("#link-clear-inline-button-template").html() );
					} else {
						clear = $( $("#link-clear-block-button-template").html() );
					}
										
					if (mode === "view") clear.hide();
					if (mode === "edit" && immutable) clear.hide();
					
					clone.append(clear);
					clear.on("click", function () {
						clone.remove();
						individual[rel_uri] = individual[rel_uri].filter(function (item) { return item.id != lnk.id; });
						if (embedded.length) {
							var index = embedded.indexOf(lnk);
							if ( index >= 0 ) embedded.splice(index, 1);
						}
					});

					function modeHandler (mode) {
						mode === "view" ? clear.hide() :
						mode === "edit" ? clear.show() :
						mode === "search" ? clear.show() :
						true;
					}
					if (!immutable) {
						individual.on("edit", modeHandler);
						individual.one("cleanup", function () {
							individual.off("edit", modeHandler);
						});
					}
					individual.on("view search", modeHandler);
					individual.one("cleanup", function () {
						individual.off("view", modeHandler);
						individual.off("search", modeHandler);
					});
					
				}, 0);
			} else {
				// External resources
				clone.append( $("<a>", {href: value, text: value}) );
			}
			relContainer.before(clone);
			return clone.show();
		}
		
	}

	function renderProperty (individual, property_uri, container, spec, mode) {
		
		if ( property_uri != "id" && !veda.ontology[property_uri] ) return;
		
		var immutable = spec && spec.hasValue("v-ui:immutable") && spec["v-ui:immutable"][0] == true;
		
		if (immutable && mode === "edit") mode = "view";
		
		container.empty();
		
		if (property_uri == "id") { 
			container.val(individual[property_uri]).html(individual[property_uri]); 
			return;
		}
		
		var property = veda.ontology[property_uri],
			controlType, emptyVal, defaultValue;
		
		if ( !individual[property_uri] ) individual.defineProperty(property_uri);
		var values = individual[property_uri];
		
		container.attr("content", values.join(", "));		

		switch (property["rdfs:range"][0].id) {
			case "xsd:boolean": 
				controlType = $.fn.vedaBoolean; 
				emptyVal = spec && spec.hasValue("v-ui:defaultBooleanValue") ? spec["v-ui:defaultBooleanValue"][0] : new Boolean(false);
				break;
			case "xsd:integer": 
			case "xsd:nonNegativeInteger":
				controlType = $.fn.vedaInteger; 
				emptyVal = spec && spec.hasValue("v-ui:defaultIntegerValue") ? spec["v-ui:defaultIntegerValue"][0] : undefined; 
				break;
			case "xsd:decimal":
				controlType = $.fn.vedaDecimal; 
				emptyVal = spec && spec.hasValue("v-ui:defaultDecimalValue") ? spec["v-ui:defaultDecimalValue"][0] : undefined; 
				break;
			case "xsd:dateTime": 
				controlType = $.fn.vedaDatetime; 
				emptyVal = spec && spec.hasValue("v-ui:defaultDatetimeValue") ? spec["v-ui:defaultDatetimeValue"][0] : undefined; 
				break;
			default: 
				controlType = $.fn.vedaString; 
				emptyVal = spec && spec.hasValue("v-ui:defaultStringValue") ? spec["v-ui:defaultStringValue"][0] : new String(); 
				break;
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
		
		if (mode !== "search" && !immutable) isValid(individual, spec, values) ? controls.map( function(item) {item.addClass("has-success");} ) : controls.map( function(item) {item.addClass("has-error");} );
		
		function modeHandler (_mode) {
			mode = _mode;
			controls
				.filter(function (item) {
					return !!item;
				})
				.map(function (item) {
					item.trigger(mode);
				});
		}
		if (!immutable) {
			individual.on("edit", modeHandler);
			individual.one("cleanup", function () {
				individual.off("edit", modeHandler);
			});
		}
		individual.on("view search", modeHandler);
		individual.one("cleanup", function () {
			individual.off("view", modeHandler);
			individual.off("search", modeHandler);
		});
		
		function renderControl (value, index) {
			var opts = {
				value: value,
				change: function (value) {
					values[index] = value;
					individual[property_uri] = values;
				},
				add: function () {
					values.push( emptyVal );
					individual[property_uri] = values;
				},
				remove: function () {
					values.splice(index, 1);
					controls.splice(index, 1);
					individual[property_uri] = values;
				}
			};
			var control = controlType.call( $("<span>"), opts );
			
			container.append(control);
			return control;
		}
		
	}

	function isValid (individual, spec, values) {
		var result = true;
		if (!spec) return result;			
		// cardinality check
		if (spec.hasValue("v-ui:minCardinality")) { 
			result = result && (
				values.length >= spec["v-ui:minCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item && !!item.valueOf();}).length
			);
		}
		if (spec.hasValue("v-ui:maxCardinality")) { 
			result = result && (
				values.length <= spec["v-ui:maxCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item && !!item.valueOf();}).length
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
					break;
				case "v-ui:DecimalPropertySpecification" :
					if (spec.hasValue("v-ui:minDecimalValue")) result = result && (value >= spec["v-ui:minDecimalValue"][0]);
					if (spec.hasValue("v-ui:maxDecimalValue")) result = result && (value <= spec["v-ui:maxDecimalValue"][0]);
					break;
				case "v-ui:DatetimePropertySpecification" :
					if (spec.hasValue("v-ui:minDatetimeValue")) result = result && (value >= spec["v-ui:minDatetimeValue"][0]);
					if (spec.hasValue("v-ui:maxDatetimeValue")) result = result && (value <= spec["v-ui:maxDatetimeValue"][0]);
					break;
				case "v-ui:StringPropertySpecification" :
					if (spec.hasValue("v-ui:minLength")) result = result && (value.length >= spec["v-ui:minLength"][0]);
					if (spec.hasValue("v-ui:maxLength")) result = result && (value.length <= spec["v-ui:maxLength"][0]);
					break;
				case "v-ui:BooleanPropertySpecification" :
				case "v-ui:ObjectPropertySpecification" :
					break;
			}
			return result;
		}, result);
		individual.isValid = individual.isValid || {};
		individual.isValid[spec.id] = result;
		individual.trigger("validation:complete");
		return result;
	}
	
	function genericTemplate (individual, _class) {
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
			properties = individual.properties;
		}
		
		$(".properties", template).append (
			Object.getOwnPropertyNames(properties).map( function (property_uri, index, array) {
				var property = veda.ontology[property_uri];
				if (property_uri === "rdfs:label" || property_uri === "rdf:type" || property_uri === "v-s:deleted") return;
				
				var result = $("<div/>").append( propTmpl );
				$(".name", result).append (
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
						$(".value", result).append (
							$("<div/>", {"property": property_uri})
						);
					break;
					default:
						$(".value", result).append (
							$("<div/>", {"rel": property_uri})
						);
					break;
				}
				
				if (index < array.length-1) result.append( $("<hr/>").attr("style", "margin: 10px 0px") );
				
				return result;
				
			})
		);
		return template;
	}
	
});



*/




















// Individual Presenter

veda.Module(function IndividualPresenter(veda) { "use strict";
	
	var deletedAlertTmpl = $("#deleted-individual-alert-template").html();
	
	veda.on("individual:loaded", function PresentIndividual(individual, container_param, template, mode) {
		
		if (!container_param) return;
		
		var uid = veda.Util.guid();
		
		var container = $(container_param);

		mode = mode || "view";
		
		container
			.empty()//.hide()
			.attr("resource", individual.id)
			.attr("typeof", individual["rdf:type"].map(function (item) { return item.id; }).join(" ") );

		// Change location.hash if individual was presented in #main container
		if (container.prop("id") === "main") {
			var hash = ["#/individual", individual.id, "#main"].join("/");
			riot.route(hash, false);
		}
			
		if (individual["v-s:deleted"] && individual["v-s:deleted"][0] == true) {
			var deletedAlert = $( deletedAlertTmpl );
			container.prepend(deletedAlert);
			$("button", deletedAlert).click(function () {
				individual.trigger("recover");
			});
		}

		var classTemplate, templateStr, specs = {}, scripts = [];
		
		if (template) {
			templateStr = template["v-ui:template"][0].toString();
			templateStr = templateStr.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
				scripts.push(script);
				return "";
			});
			classTemplate = $( templateStr );
			specs = $.extend.apply (this, [specs].concat(
				individual["rdf:type"]
					.filter( function (_class) {
						return _class instanceof veda.IndividualModel;
					})
					.map( function (_class) {
						return _class.specsByProps;
					})
				)
			);
			renderTemplate (individual, container, classTemplate, specs, scripts, mode);

		} else if (
			!individual["rdf:type"]
				.filter( function (_class) {
					return _class instanceof veda.IndividualModel;
				})
				.map( function (_class) {
					if (_class.template && _class.template["v-ui:template"]) {
						// If _class.template is embedded => construct generic template
						if (_class.template["v-ui:embedded"] && _class.template["v-ui:embedded"][0] == true) {
							classTemplate = genericTemplate(individual, _class); 
						} else {
							// Get template from class
							templateStr = _class.template["v-ui:template"][0].toString();
							templateStr = templateStr.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
								scripts.push(script);
								return "";
							});
							classTemplate = $( templateStr );
						}
					} else {
						// Construct generic template
						classTemplate = genericTemplate(individual, _class);
					}
					specs = _class.specsByProps || {};
					renderTemplate (individual, container, classTemplate, specs, scripts, mode);
				})
				.length
		) {
			classTemplate = genericTemplate(individual);
			renderTemplate (individual, container, classTemplate, specs, scripts, mode);
		}
	});
	
	function renderTemplate (individual, container, classTemplate, specs, scripts, mode) {

		// Cleanup memory
		classTemplate.on("remove", function (event) {
			individual.trigger("cleanup");
			$(".typeahead", container).typeahead("destroy");
			individual = embedded = container = classTemplate = specs = scripts = mode = null;
		});
		
		// Embedded templates list
		var embedded = [];
				
		// Trigger same events for embedded templates
		function syncEmbedded (event) {
			embedded.map(function (item) {
				if (item.trigger) item.trigger(event);
			});
		}
		individual.on("view edit save delete recover", syncEmbedded);
		individual.one("cleanup", function () {
			individual.off("view", syncEmbedded);
			individual.off("edit", syncEmbedded);
			individual.off("save", syncEmbedded);
			individual.off("delete", syncEmbedded);
			individual.off("recover", syncEmbedded);
		});

		// Define handlers
		function saveHandler () {
			individual.save();
			individual.trigger("view");
			// Change location.hash if individual was presented in #main container
			if (container.prop("id") === "main") {
				var hash = ["#/individual", individual.id, "#main"].join("/");
				riot.route(hash, false);
			}
		}
		individual.on("save", saveHandler);
		individual.one("cleanup", function () {
			individual.off("save", saveHandler);
		});

		function cancelHandler () {
			embedded.map(function (item) {
				if (item.reset) item.reset();
			});
			individual.reset();
			veda.trigger("individual:loaded", individual, container);//, classTemplate);
		}
		individual.on("cancel", cancelHandler);
		individual.one("cleanup", function () {
			individual.off("cancel", cancelHandler);
		});
		
		function deleteHandler () {
			individual.delete();
			individual.trigger("cancel");
		}
		individual.on("delete", deleteHandler);
		individual.one("cleanup", function () {
			individual.off("delete", deleteHandler);
		});

		function recoverHandler () {
			individual.recover();
			individual.trigger("cancel");
		}
		individual.on("recover", recoverHandler);
		individual.one("cleanup", function () {
			individual.off("recover", recoverHandler);
		});

		function typeChangeHandler () {
			veda.trigger("individual:loaded", individual, container);//, classTemplate);
		}
		individual.on("individual:typeChanged", typeChangeHandler);
		individual.one("cleanup", function () {
			individual.off("individual:typeChanged", typeChangeHandler);
		});
		
		// Actions
		var $edit = $("#edit.action", classTemplate),
			$save = $("#save.action", classTemplate),
			$cancel = $("#cancel.action", classTemplate),
			$delete = $("#delete.action", classTemplate),
			$search = $("#search.action", classTemplate);

		// Check rights to manage buttons		
		// Update
		if ($edit.length   && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $edit.remove();
		if ($save.length   && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $save.remove();
		if ($cancel.length && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $cancel.remove();
		if ($delete.length && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $delete.remove();
		// Delete
		if ($delete.length && !(individual.rights.hasValue("v-s:canDelete") && individual.rights["v-s:canDelete"][0] == true) ) $delete.remove();

		// Show / hide buttons in different modes
		function modeHandler (_mode) {
			mode = _mode;
			mode === "view"   ? ( $edit.show(), $save.hide(), $cancel.hide(), $delete.show(), $search.hide() ) :
			mode === "edit"   ? ( $edit.hide(), $save.show(), $cancel.show(), $delete.show(), $search.hide() ) :
			mode === "search" ? ( $edit.hide(), $save.hide(), $cancel.hide(), $delete.hide(), $search.show() ) : 
			true;
		}
		individual.on("view edit search", modeHandler);
		individual.one("cleanup", function () {
			individual.off("view", modeHandler);
			individual.off("edit", modeHandler);
			individual.off("search", modeHandler);
		});

		// Buttons handlers
		// Edit
		$edit.on("click", function (e) {
			individual.trigger("edit");
		});
		
		// Save
		$save.on("click", function (e) {
			individual.trigger("save");
		});

		function validationHandler () {
			var isValid = Object.keys(individual.isValid).reduce(function (state, specId) {
				return state && individual.isValid[specId];
			}, true);
			isValid ? $save.removeAttr("disabled") : $save.attr("disabled", "disabled");
		}
		individual.on("validation:complete", validationHandler);
		individual.one("cleanup", function () {
			individual.off("validation:complete", validationHandler);
		});

		
		//  Cancel
		$cancel.on("click", function (e) {
			individual.trigger("cancel");
		});
		
		//  Delete
		$delete.on("click", function (e) {
			if ( confirm("Вы действительно хотите удалить документ?") ) individual.trigger("delete");
		});
		if (individual.hasValue("v-s:deleted") && individual["v-s:deleted"][0]) $delete.hide();
		
		// Search
		$search.on("click", function (e) {
			// serialize individual as search query
			var query;
			var allProps = Object.getOwnPropertyNames(individual.properties)
				.map(function (property_uri) {
					var property = individual.properties[property_uri];
					var values = individual[property_uri].filter(function(item){return !!item && !!item.valueOf();});
					var oneProp;
					switch (property["rdfs:range"][0].id) {
						case "xsd:integer": 
						case "xsd:nonNegativeInteger":
						case "xsd:decimal":
							oneProp =
								values.length === 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[0] + "]" :
								values.length > 1 ? "'" + property_uri + "'==[" + values[0] + "," + values[values.length-1] + "]" :
								undefined;
							break;
						case "xsd:dateTime": 
							oneProp =
								values.length === 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[0].toISOString().substring(0,19) + "]" :
								values.length > 1 ? "'" + property_uri + "'==[" + values[0].toISOString().substring(0,19) + "," + values[values.length-1].toISOString().substring(0,19) + "]" :
								undefined;
							break;
						case "xsd:boolean": 
						case "xsd:string": 
						case "rdfs:Literal": 
							oneProp = values
								.filter(function(item){return !!item && !!item.valueOf();})
								.map( function (value) {
									return "'" + property_uri + "'=='" + value + "'";
								})
								.join("||");
							break;
						default:
							oneProp = values
								.filter( function (value) {
									return value instanceof veda.IndividualModel;
								})
								.map( function (value) {
									return "'" + property_uri + "'=='" + value.id + "'";
								})
								.join("||");
							break;
					}
					return oneProp ? "(" + oneProp + ")" : undefined;
				})
				.filter(function(item){return !!item;})
				.join("&&");
			query = allProps ? "(" + allProps + ")" : undefined;
			
			var individual_uri = individual.id;
			// Open Search
			var search = new veda.SearchModel(query);
			// Place individual to params tab in Search container
			var params = new veda.IndividualModel(individual_uri, $("#params-" + search.id, search.view), undefined, "search");
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
				relTemplate = relContainer.attr("template"),
				spec = specs[rel_uri];
			
			relContainer.empty().hide();
			
			relTemplate = relTemplate ? (
				new veda.IndividualModel(relTemplate) 
			) : (
				individual.hasValue(rel_uri) && individual[rel_uri][0].hasValue("rdfs:label") ? 
					new veda.IndividualModel("v-ui:ClassNameLabelTemplate")
					:
					new veda.IndividualModel("v-ui:ClassNameIdTemplate")
			);
			var rendered = renderLink(individual, rel_uri, relContainer, relTemplate, spec, mode, embedded);
			
			// Re-render link property if its' values were changed
			function propModHandler (doc_property_uri) {
				if (doc_property_uri === rel_uri) {
					rendered.map( function (item) { item.remove(); } );
					rendered = renderLink(individual, rel_uri, relContainer, relTemplate, spec, mode, embedded);
				}
			}
			individual.on("individual:propertyModified", propModHandler);
			individual.one("cleanup", function () {
				individual.off("individual:propertyModified", propModHandler);
			});

		});
		
		// Properties
		$("[property]", classTemplate).not("[about]").map( function () {
			
			var propertyContainer = $(this),
				property_uri = propertyContainer.attr("property"),
				//propertyTemplate = propertyContainer.attr("template"),
				spec = specs[property_uri];
				
			renderProperty(individual, property_uri, propertyContainer, spec, mode);
			
			// Re-render property if its' values were changed
			function propModHandler (doc_property_uri) {
				if (doc_property_uri === property_uri) {
					renderProperty (individual, property_uri, propertyContainer, spec, mode);
				}
			}
			individual.on("individual:propertyModified", propModHandler);
			individual.one("cleanup", function () {
				individual.off("individual:propertyModified", propModHandler);
			});
			
		});
		
		// Specials (not RDFa)
		$("[href='id']", classTemplate).map( function () {
			$( this )
				.attr("href", "#/individual/" + individual.id)
				.after( 
					$("<a>", {href: "#/graph/" + individual.id}).append( 
						$("<i>").addClass("glyphicon glyphicon-link") 
					) 
				)
				.after( "&nbsp;" );
		});
		
		container.append(classTemplate);
		
		individual.trigger(mode);
		
		//container.show();
		
		scripts.map( function (item) { 
			var presenter = new Function("veda", "individual", "container", item + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js");
			presenter(veda, individual, classTemplate);
		});
	}
	
	function renderLink (individual, rel_uri, relContainer, relTemplate, spec, mode, embedded) {
		
		if ( !individual[rel_uri] ) individual.defineProperty(rel_uri);
		
		function insDefObjVal () {
			individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
		}
		if ( spec && spec.hasValue("v-ui:defaultObjectValue") && !individual.hasValue(rel_uri) ) {
			individual.on("edit", insDefObjVal);
			individual.one("cleanup", function () {
				individual.off("edit", insDefObjVal);
			});
		}
		
		var immutable = spec && spec.hasValue("v-ui:immutable") && spec["v-ui:immutable"][0] == true;

		var values = individual[rel_uri];
		
		var renderedValues = individual.hasValue(rel_uri) ? 
			individual[rel_uri].map( function (value) { return renderValue (value, mode);} ) : [];

		var controlContainer;
		if (renderedValues.length) {
			controlContainer = renderedValues[renderedValues.length-1];
		} else {
			controlContainer = relContainer.clone();
			relContainer.after(controlContainer);
		}
		
		var opts = {
			limit: 100,
			select: function (selected) {
				individual[rel_uri] = individual[rel_uri].concat(selected);
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
				individual[rel_uri] = individual[rel_uri].concat(embedded);
			};
		}
		var control = $("<span>").vedaLink(opts);

		if (mode === "view") control.hide();
		if (mode === "edit" && immutable) control.hide();
		
		// tooltip from spec
		if (spec && spec.hasValue("v-ui:tooltip")) {
			control.tooltip({
				title: spec["v-ui:tooltip"].join(", "),
				placement: "auto bottom",
				container: control,
				trigger: "focus"
			});
		}
		
		if (mode !== "search" && !immutable) isValid(individual, spec, values) ? control.addClass("has-success") : control.addClass("has-error") ;
		
		setTimeout( function () {
			controlContainer.append(control).show();
		}, 0);

		function modeHandler (mode) {
			mode === "view" ? control.hide() : 
			mode === "edit" ? control.show() : 
			mode === "search" ? control.show() : 
			true;
		}
		if (!immutable) {
			individual.on("edit", modeHandler);
			individual.one("cleanup", function () {
				individual.off("edit", modeHandler);
			});
		}
		individual.on("view search", modeHandler);
		individual.one("cleanup", function () {
			individual.off("view", modeHandler);
			individual.off("search", modeHandler);
		});
		
		return [controlContainer].concat(renderedValues);

		function renderValue(value, mode) {
			// Create the same tag container to preserve element layout
			var clone = relContainer.clone();
			var lnk;
			if (value instanceof veda.IndividualModel || !value) {
				setTimeout( function () {
					if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) {
						lnk = new veda.IndividualModel(value, clone, relTemplate, mode);
						embedded.push(lnk);
						// New instance
						if (!value) individual[rel_uri] = individual[rel_uri].concat(lnk);
					} else {
						lnk = new veda.IndividualModel(value, clone, relTemplate);
					}
					
					clone.attr("style", "position:relative;");
					var clear = $( $("#link-clear-button-template").html() );
					
					if (clone.children(":first").css("display") === "inline") {
						clear = $( $("#link-clear-inline-button-template").html() );
					} else {
						clear = $( $("#link-clear-block-button-template").html() );
					}
										
					if (mode === "view") clear.hide();
					if (mode === "edit" && immutable) clear.hide();
					
					clone.append(clear);
					clear.on("click", function () {
						clone.remove();
						individual[rel_uri] = individual[rel_uri].filter(function (item) { return item.id != lnk.id; });
						if (embedded.length) {
							var index = embedded.indexOf(lnk);
							if ( index >= 0 ) embedded.splice(index, 1);
						}
					});

					function modeHandler (mode) {
						mode === "view" ? clear.hide() :
						mode === "edit" ? clear.show() :
						mode === "search" ? clear.show() :
						true;
					}
					if (!immutable) {
						individual.on("edit", modeHandler);
						individual.one("cleanup", function () {
							individual.off("edit", modeHandler);
						});
					}
					individual.on("view search", modeHandler);
					individual.one("cleanup", function () {
						individual.off("view", modeHandler);
						individual.off("search", modeHandler);
					});
					
				}, 0);
			} else {
				// External resources
				clone.append( $("<a>", {href: value, text: value}) );
			}
			relContainer.before(clone);
			return clone.show();
		}
		
	}

	function renderProperty (individual, property_uri, container, spec, mode) {
		
		if ( property_uri != "id" && !veda.ontology[property_uri] ) return;
		
		var immutable = spec && spec.hasValue("v-ui:immutable") && spec["v-ui:immutable"][0] == true;
		
		if (immutable && mode === "edit") mode = "view";
		
		container.empty();
		
		if (property_uri == "id") { 
			container.val(individual[property_uri]).html(individual[property_uri]); 
			return;
		}
		
		var property = veda.ontology[property_uri],
			controlType, emptyVal, defaultValue;
		
		if ( !individual[property_uri] ) individual.defineProperty(property_uri);
		var values = individual[property_uri];
		
		container.attr("content", values.join(", "));		

		switch (property["rdfs:range"][0].id) {
			case "xsd:boolean": 
				controlType = $.fn.vedaBoolean; 
				emptyVal = spec && spec.hasValue("v-ui:defaultBooleanValue") ? spec["v-ui:defaultBooleanValue"][0] : new Boolean(false);
				break;
			case "xsd:integer": 
			case "xsd:nonNegativeInteger":
				controlType = $.fn.vedaInteger; 
				emptyVal = spec && spec.hasValue("v-ui:defaultIntegerValue") ? spec["v-ui:defaultIntegerValue"][0] : undefined; 
				break;
			case "xsd:decimal":
				controlType = $.fn.vedaDecimal; 
				emptyVal = spec && spec.hasValue("v-ui:defaultDecimalValue") ? spec["v-ui:defaultDecimalValue"][0] : undefined; 
				break;
			case "xsd:dateTime": 
				controlType = $.fn.vedaDatetime; 
				emptyVal = spec && spec.hasValue("v-ui:defaultDatetimeValue") ? spec["v-ui:defaultDatetimeValue"][0] : undefined; 
				break;
			default: 
				controlType = $.fn.vedaString; 
				emptyVal = spec && spec.hasValue("v-ui:defaultStringValue") ? spec["v-ui:defaultStringValue"][0] : new String(); 
				break;
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
		
		if (mode !== "search" && !immutable) isValid(individual, spec, values) ? controls.map( function(item) {item.addClass("has-success");} ) : controls.map( function(item) {item.addClass("has-error");} );
		
		function modeHandler (_mode) {
			mode = _mode;
			controls
				.filter(function (item) {
					return !!item;
				})
				.map(function (item) {
					item.trigger(mode);
				});
		}
		if (!immutable) {
			individual.on("edit", modeHandler);
			individual.one("cleanup", function () {
				individual.off("edit", modeHandler);
			});
		}
		individual.on("view search", modeHandler);
		individual.one("cleanup", function () {
			individual.off("view", modeHandler);
			individual.off("search", modeHandler);
		});
		
		function renderControl (value, index) {
			var opts = {
				value: value,
				change: function (value) {
					values[index] = value;
					individual[property_uri] = values;
				},
				add: function () {
					values.push( emptyVal );
					individual[property_uri] = values;
				},
				remove: function () {
					values.splice(index, 1);
					controls.splice(index, 1);
					individual[property_uri] = values;
				}
			};
			var control = controlType.call( $("<span>"), opts );
			
			container.append(control);
			return control;
		}
		
	}

	function isValid (individual, spec, values) {
		var result = true;
		if (!spec) return result;			
		// cardinality check
		if (spec.hasValue("v-ui:minCardinality")) { 
			result = result && (
				values.length >= spec["v-ui:minCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item && !!item.valueOf();}).length
			);
		}
		if (spec.hasValue("v-ui:maxCardinality")) { 
			result = result && (
				values.length <= spec["v-ui:maxCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item && !!item.valueOf();}).length
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
					break;
				case "v-ui:DecimalPropertySpecification" :
					if (spec.hasValue("v-ui:minDecimalValue")) result = result && (value >= spec["v-ui:minDecimalValue"][0]);
					if (spec.hasValue("v-ui:maxDecimalValue")) result = result && (value <= spec["v-ui:maxDecimalValue"][0]);
					break;
				case "v-ui:DatetimePropertySpecification" :
					if (spec.hasValue("v-ui:minDatetimeValue")) result = result && (value >= spec["v-ui:minDatetimeValue"][0]);
					if (spec.hasValue("v-ui:maxDatetimeValue")) result = result && (value <= spec["v-ui:maxDatetimeValue"][0]);
					break;
				case "v-ui:StringPropertySpecification" :
					if (spec.hasValue("v-ui:minLength")) result = result && (value.length >= spec["v-ui:minLength"][0]);
					if (spec.hasValue("v-ui:maxLength")) result = result && (value.length <= spec["v-ui:maxLength"][0]);
					break;
				case "v-ui:BooleanPropertySpecification" :
				case "v-ui:ObjectPropertySpecification" :
					break;
			}
			return result;
		}, result);
		individual.isValid = individual.isValid || {};
		individual.isValid[spec.id] = result;
		individual.trigger("validation:complete");
		return result;
	}
	
	function genericTemplate (individual, _class) {
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
			properties = individual.properties;
		}
		
		$(".properties", template).append (
			Object.getOwnPropertyNames(properties).map( function (property_uri, index, array) {
				var property = veda.ontology[property_uri];
				if (property_uri === "rdfs:label" || property_uri === "rdf:type" || property_uri === "v-s:deleted") return;
				
				var result = $("<div/>").append( propTmpl );
				$(".name", result).append (
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
						$(".value", result).append (
							$("<div/>", {"property": property_uri})
						);
					break;
					default:
						$(".value", result).append (
							$("<div/>", {"rel": property_uri})
						);
					break;
				}
				
				if (index < array.length-1) result.append( $("<hr/>").attr("style", "margin: 10px 0px") );
				
				return result;
				
			})
		);
		return template;
	}
	
});
