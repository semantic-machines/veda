// Individual Presenter

veda.Module(function IndividualPresenter(veda) { "use strict";
	
	var deletedAlertTmpl = $("#deleted-individual-alert-template").html();
	
	//var c = 0;
	
	veda.on("individual:loaded", function (individual, container, template, mode) {
		
		//console.log(individual.id, "presenter count:", ++c);
		
		if (typeof container === "string") container = $(container);
	
		container
			.empty()
			.attr("resource", individual.id)
			.attr("typeof", individual["rdf:type"].map(function (item) { return item.id; }).join(" ") );

		mode = mode || "view";

		// Change location.hash if individual was presented in #main container
		if (container.prop("id") === "main") {
			var hash = ["#", "individual", individual.id, "#main"].join("/");
			if (hash !== location.hash) riot.route(hash, false);
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
				$scripts.map(function () { scripts.push( $(this).text() );});
				$scripts.remove();
			}
			if (template instanceof veda.IndividualModel) {
				template = template["v-ui:template"][0].toString();
				template = template.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
					scripts.push(script);
					return "";
				});
				template = $(template);
			}
			if (template instanceof String || typeof template === "string") {
				template = template.replace(/<script.*>((?:\s*?.*?\s*?)*)<\/script>/gi, function (m, script) {
					scripts.push(script);
					return "";
				});
				template = $(template);
			}
			rendered.push({
				template: renderTemplate(individual, container, template, specs, mode),
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
						template: renderTemplate(individual, container, template, specs, mode),
						scripts: scripts
					};
				});
		}
		
		if (!rendered.length) {
			template = genericTemplate(individual);
			rendered.push({
				template: renderTemplate(individual, container, template, specs, mode),
				scripts: scripts
			});
		}
		
		rendered.map( function (view) {
			setTimeout(function () {
				view.template.trigger(mode);	
				container.prepend(view.template);
				view.scripts.map( function (script) { 
					var presenter = new Function("veda", "individual", "template", "container", script + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js");
					presenter(veda, individual, view.template, container);
				});
			}, 0);
		});
		
	});
	
	function renderTemplate (individual, container, template, specs, mode) {

		// Cleanup memory
		template.on("remove", function (event) {
			$(".typeahead", template).typeahead("destroy");
			//individual = embedded = template = specs = null;
		});
		
		// Embedded templates list
		var embedded = [];

		// Trigger same events for embedded templates
		function syncEmbedded (e) {
			embedded.map(function (item) {
				item.trigger(e.type);
			});
			e.stopPropagation();
		}
		template.on("view edit search save cancel delete recover", syncEmbedded);
				
		// Define handlers
		function saveHandler (e) {
			individual.save();
			template.trigger("view");
			// Change location.hash if individual was presented in #main container
			if (container.prop("id") === "main") {
				var hash = ["#", "individual", individual.id, "#main"].join("/");
				if (hash !== location.hash) riot.route(hash, false);
			}
			e.stopPropagation();
		}
		template.on("save", saveHandler);

		function cancelHandler (e) {
			individual.reset();
			veda.trigger("individual:loaded", individual, container);
			e.stopPropagation();
		}
		template.on("cancel", cancelHandler);

		// Deleted alert
		var deletedAlert = $( deletedAlertTmpl );
		$("button", deletedAlert).click(function () {
			template.trigger("recover");
		});
		if (individual.hasValue("v-s:deleted") && individual["v-s:deleted"][0] == true) {
			afterDeleteHandler();
		}
		function afterRecoverHandler() {
			deletedAlert.detach();
		}
		function afterDeleteHandler() {
			template.prepend(deletedAlert);
		}
		individual.on("individual:afterRecover", afterRecoverHandler);
		individual.on("individual:afterDelete", afterDeleteHandler);
		template.one("remove", function () {
			individual.off("individual:afterRecover", afterRecoverHandler);
			individual.off("individual:afterDelete", afterDeleteHandler);
		});

		function deleteHandler (e) {
			individual.delete();
			e.stopPropagation();
		}
		template.on("delete", deleteHandler);

		function recoverHandler (e) {
			individual.recover();
			e.stopPropagation();
		}
		template.on("recover", recoverHandler);

		/*function typeChangeHandler () {
			veda.trigger("individual:loaded", individual, container, undefined, mode);
		}
		individual.on("individual:typeChanged", typeChangeHandler);
		template.one("remove", function () {
			individual.off("individual:typeChanged", typeChangeHandler);
		});*/
		
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
		function modeHandler (e) {
			mode = e.type;
			e.type === "view"   ? ( $edit.show(), $save.hide(), $cancel.hide(), $delete.show(), $search.hide() ) :
			e.type === "edit"   ? ( $edit.hide(), $save.show(), $cancel.show(), $delete.show(), $search.hide() ) :
			e.type === "search" ? ( $edit.hide(), $save.hide(), $cancel.hide(), $delete.hide(), $search.show() ) : 
			true;
			e.stopPropagation();
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
		individual.on("validation:complete", validationHandler);
		template.one("remove", function () {
			individual.off("validation:complete", validationHandler);
		});
		
		//  Cancel
		$cancel.on("click", function (e) {
			template.trigger("cancel");
		});
		
		//  Delete
		$delete.on("click", function (e) {
			if ( confirm("Вы действительно хотите удалить документ?") ) template.trigger("delete");
		});
		if (individual.hasValue("v-s:deleted") && individual["v-s:deleted"][0]) $delete.hide();
		
		// Search
		$search.on("click", function (e) {
			var query = queryFromIndividual(individual);
			var individual_uri = individual.id;
			// Create Search instance
			var search = new veda.SearchModel(query);
			// Place individual to params tab in Search container
			var params = new veda.IndividualModel(individual_uri, $("#params-" + search.id, search.view), undefined, "search");
		});
		
		// Process RDFa compliant template
		// About resources
		$("[about]", template).map( function () {
			
			var propertyContainer = $(this), 
				about = new veda.IndividualModel(propertyContainer.attr("about")),
				property_uri = propertyContainer.attr("property");
			if (property_uri == "id") propertyContainer.html( about[property_uri] );
			else propertyContainer.html( about[property_uri].join(", ") );
		});
		
		// Related resources
		$("[rel]", template).map( function () {
			
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
			var rendered = renderRelation(individual, rel_uri, relContainer, relTemplate, spec, embedded, template, mode, propertyModifiedHandler);
			
			// Re-render link property if its' values were changed
			function propertyModifiedHandler (doc_property_uri) {
				if (doc_property_uri === rel_uri) {
					rendered.map( function (item) { item.remove(); } );
					rendered = renderRelation(individual, rel_uri, relContainer, relTemplate, spec, embedded, template, mode, propertyModifiedHandler);
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});

		});
		
		// Properties
		$("[property]", template).not("[about]").map( function () {
			
			var propertyContainer = $(this),
				property_uri = propertyContainer.attr("property"),
				//propertyTemplate = propertyContainer.attr("template"),
				spec = specs[property_uri];
				
			renderProperty(individual, property_uri, propertyContainer, spec, template, mode, propertyModifiedHandler);
			
			// Re-render property if its' values were changed
			function propertyModifiedHandler (doc_property_uri) {
				if (doc_property_uri === property_uri) {
					renderProperty (individual, property_uri, propertyContainer, spec, template, mode, propertyModifiedHandler);
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
			
		});
		
		// Specials (not RDFa)
		$("[href='id']", template).map( function () {
			$(this)
				.attr("href", "#/individual/" + individual.id + "/#main")
				.after( 
					$("<a>", {href: "#/graph/" + individual.id}).append( 
						$("<i>").addClass("glyphicon glyphicon-link") 
					) 
				)
				.after( "&nbsp;" );
		});
		
		return template;
	}
	
	function renderRelation (individual, rel_uri, relContainer, relTemplate, spec, embedded, template, mode, handler) {
		
		if ( !individual[rel_uri] ) individual.defineProperty(rel_uri);
		
		function assignDefaultObjectValue (e) {
			individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
			e.stopPropagation();
		}
		if ( spec && spec.hasValue("v-ui:defaultObjectValue") && !individual.hasValue(rel_uri) ) {
			template.on("edit", assignDefaultObjectValue);
		}
		
		var immutable = spec && spec.hasValue("v-ui:immutable") && spec["v-ui:immutable"][0] == true;
		
		var values = individual[rel_uri];
		
		var renderedValues = individual.hasValue(rel_uri) ? 
			individual[rel_uri].map( function (value) { return renderValue (value); } ) : [];

		var controlContainer = relContainer.clone();
		relContainer.after(controlContainer);
		
		var opts = {
			limit: 100,
			select: function (selected) {
				individual[rel_uri] = individual[rel_uri].concat(selected);
			}
		};
		if (spec && spec.hasValue("v-ui:queryPrefix")) {
			opts.queryPrefix = spec["v-ui:queryPrefix"][0];
		}
		if (relTemplate.hasValue("v-ui:embedded") && relTemplate["v-ui:embedded"][0] == true) {
			
			opts.add = function () {
				var embeddedIndividual = new veda.IndividualModel();
				if (relTemplate.hasValue("v-ui:forClass")) {
					embeddedIndividual["rdf:type"] = [relTemplate["v-ui:forClass"][0]];
				}
				individual[rel_uri] = individual[rel_uri].concat(embeddedIndividual);
			};
		}
		
		var control = controlContainer.css("display") === "inline" ? 
			$("<span>").vedaLink(opts) : $("<div>").vedaLink(opts);

		// tooltip from spec
		if (spec && spec.hasValue("v-ui:tooltip")) {
			control.tooltip({
				title: spec["v-ui:tooltip"].join(", "),
				placement: "auto bottom",
				container: control,
				trigger: "focus"
			});
		}
		
		controlContainer.append(control);
		
		mode === "view" ? controlContainer.hide() :
		mode === "edit" ? controlContainer.show() :
		mode === "search" ? controlContainer.show() : true;

		function modeHandler (e) {
			mode = e.type;
			e.type === "view" ? controlContainer.hide() : 
			e.type === "edit" && !immutable ? controlContainer.show() : 
			e.type === "edit" && immutable ? controlContainer.hide() : 
			e.type === "search" ? controlContainer.show() : 
			true;
			if (e.type === "edit") isValid(individual, spec, values) ? control.addClass("has-success") : control.addClass("has-error") ;
			e.stopPropagation();
		}
		template.on("view edit search", modeHandler);

		if (mode === "edit") isValid(individual, spec, values) ? control.addClass("has-success") : control.addClass("has-error") ;
		
		return [controlContainer].concat(renderedValues);

		function renderValue(value) {
			// Create the same tag container to preserve element layout
			var clone = relContainer.clone();
			var lnk;
			var lnkTemplate;
			if (value instanceof veda.IndividualModel || !value) {
				setTimeout( function () {
					lnkTemplate = $("<span>").append( $( relTemplate["v-ui:template"][0].toString() ) );
					if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0] == true) {
						lnk = new veda.IndividualModel(value, clone, lnkTemplate, mode);
						embedded.push(lnkTemplate);
					} else {
						lnk = new veda.IndividualModel(value, clone, lnkTemplate);
					}
					
					clone.attr("style", "position:relative;");
					
					var clear;
					if (lnkTemplate.children(":first").prop("tagName") === "SPAN") {
						clear = $( $("#link-clear-inline-button-template").html() );
					} else {
						clear = $( $("#link-clear-block-button-template").html() );
					}
										
					clear.on("click", function () {
						clone.remove();
						individual[rel_uri] = individual[rel_uri].filter(function (item) { return item.id != lnk.id; });
						if (embedded.length) {
							var index = embedded.indexOf(lnkTemplate);
							if ( index >= 0 ) embedded.splice(index, 1);
						}
					});

					mode === "view" ? clear.hide() :
					mode === "edit" ? clear.show() :
					mode === "search" ? clear.show() : true;

					function modeHandler (e) {
						e.type === "view" ? clear.hide() :
						e.type === "edit" && !immutable ? clear.show() :
						e.type === "edit" && immutable ? clear.hide() :
						e.type === "search" ? clear.show() :
						true;
						e.stopPropagation();						
					}
					template.on("view edit search", modeHandler);

					clone.append(clear);
					
				}, 0);
			} else {
				// External resources
				clone.append( $("<a>", {href: value, text: value}) );
			}
			relContainer.before(clone);
			return clone.show();
		}
		
	}

	function renderProperty (individual, property_uri, container, spec, template, mode, handler) {
		
		if ( property_uri != "id" && !veda.ontology[property_uri] ) return;
		
		var immutable = spec && spec.hasValue("v-ui:immutable") && spec["v-ui:immutable"][0] == true;
		
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
	
		function modeHandler (e) {
			mode = e.type;
			controls
				.filter(function (item) {
					return !!item;
				})
				.map(function (item) {
					item.trigger(e.type);
				});
			if (e.type === "edit") isValid(individual, spec, values) ? controls.map( function(item) {item.addClass("has-success");} ) : controls.map( function(item) {item.addClass("has-error");} );
			e.stopPropagation();				
		}
		template.on("view edit search", modeHandler);

		if (mode === "edit") isValid(individual, spec, values) ? controls.map( function(item) {item.addClass("has-success");} ) : controls.map( function(item) {item.addClass("has-error");} );
		
		function renderControl (value, index) {
			var opts = {
				immutable: immutable,
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
			if (property_uri === "v-s:script" || property_uri === "v-ui:template") {
				controlType = $.fn.vedaSource;
				opts.change = function (value) {
					individual.off("individual:propertyModified", handler);
					individual[property_uri] = [value];
					individual.on("individual:propertyModified", handler);
				}
				if (property_uri === "v-s:script") opts.mode = "javascript";
				if (property_uri === "v-ui:template") opts.mode = "htmlmixed";
			}
			
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

function queryFromIndividual(individual) {
	// Serialize individual as search query
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
	return query;
}
