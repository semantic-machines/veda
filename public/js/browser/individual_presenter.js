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

		var specs = $.extend.apply ({}, [].concat(
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
			if (template instanceof veda.IndividualModel) template = $( template["v-ui:template"][0].toString() );
			if (template instanceof String) template = $( template.toString() );
			if (typeof template === "string") template = $( template );
			var $scripts = template.filter("script");
			$scripts.map(function () { scripts.push( $(this).text() ); });
			template = template.first();
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
						// Get template from class
						template = $( _class.template["v-ui:template"][0].toString() );
						var $scripts = template.filter("script");
						$scripts.map(function () { scripts.push( $(this).text() ); });
						template = template.first();
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
			view.template.trigger(mode);	
			container.prepend(view.template);
			view.scripts.map( function (script) { 
				var presenter = new Function("veda", "individual", "template", "container", script + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js");
				presenter(veda, individual, view.template, container);
			});
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
			var params = individual;
			// Create Search instance
			var search = new veda.SearchModel(query);
			// Place individual to params tab in Search container
			params.present($("#params-" + search.id, search.view), undefined, "search");
		});

		var view = $(".view", template),
			edit = $(".edit", template),
			search = $(".search", template),
			notView = $(".-view", template),
			notEdit = $(".-edit", template),
			notSearch = $(".-search", template);
			
		// Show/hide elements with special classes in different modes
		function modeHandler (e) {
			mode = e.type;
			mode === "view" ? view.show() && notView.hide() : 
			mode === "edit" ? edit.show() && notEdit.hide() : 
			mode === "search" ? search.show() && notSearch.hide() : true;
			e.stopPropagation();
		}
		template.on("view edit search", modeHandler);
		
		// Process RDFa compliant template
		// About resource
		$("[about]", template).map( function () {
			var propertyContainer = $(this), 
				property_uri = propertyContainer.attr("property"),
				about;
			if (propertyContainer.attr("about") === "@") {
				about = individual;
				propertyContainer.attr("about", about.id);
			} else {
				about = new veda.IndividualModel(propertyContainer.attr("about"));
			}
			propertyModifiedHandler(property_uri);
			function propertyModifiedHandler(doc_property_uri) {
				if (doc_property_uri === property_uri) {
					if (property_uri === "id") propertyContainer.prepend( about.id );
					else propertyContainer.html( about[property_uri].join(", ") );
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
		});
		
		// Special (not RDFa)
		$("[href='id']", template).map( function () {
			$(this)
				.attr("href", "#/individual/" + individual.id + "/#main")
				//.after( 
				//	$("<a>", {href: "#/graph/" + individual.id}).append( 
				//		$("<i>").addClass("glyphicon glyphicon-link") 
				//	) 
				//)
				//.after( "&nbsp;" );
		});
		
		// Relation value
		$("[rel]", template).not("veda-control").map( function () {
			var relContainer = $(this), 
				rel_uri = relContainer.attr("rel"),
				isEmbedded = relContainer.attr("embedded"),
				spec = specs[rel_uri],
				rel_inline_template = relContainer.children(),
				rel_template_uri = relContainer.attr("template"),
				relTemplate;
			if ( rel_template_uri ) {
				var templateIndividual = new veda.IndividualModel( rel_template_uri );
				relTemplate = $( templateIndividual["v-ui:template"][0].toString() );
			}
			if ( rel_inline_template.length ) {
				relTemplate = rel_inline_template.clone();
			}
			//rel_inline_template.remove();
			relContainer.empty().hide();
			var rendered = renderRelationValues(individual, rel_uri, relContainer, relTemplate, isEmbedded, spec, embedded, template, mode);
			// Re-render link property if its' values were changed
			function propertyModifiedHandler (doc_property_uri) {
				if (doc_property_uri === rel_uri) {
					rendered.map( function (item) { item.remove(); } );
					rendered = renderRelationValues(individual, rel_uri, relContainer, relTemplate, isEmbedded, spec, embedded, template, mode);
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
		});		
		
		// Property value
		$("[property]", template).not("veda-control").not("[about]").map( function () {
			
			var propertyContainer = $(this).hide(),
				property_uri = propertyContainer.attr("property"),
				spec = specs[property_uri];
			
			if (property_uri == "id") { 
				propertyContainer.show().text(individual[property_uri]); 
				return;
			}

			if (!individual.hasValue(property_uri)) {
				individual.defineProperty(property_uri);
			}
			var rendered = renderPropertyValues();

			function renderPropertyValues() {
				return individual[property_uri].map( function (value, i) {
					var result = propertyContainer.clone().show().insertBefore(propertyContainer);
					var valueHolder = $("<span>");
					result.prepend(valueHolder.text(value.toString()));

					var btnRemove = $("<button class='btn btn-link btn-xs -view edit search' id='value-remove'><i class='glyphicon glyphicon-remove text-danger'></i></button>");
					var btnEdit = $("<button class='btn btn-link btn-xs -view edit search' id='value-edit'><i class='glyphicon glyphicon-edit text-primary'></i></button>");
					if (mode === "view") {
						btnRemove.hide();
						btnEdit.hide();
					}
					notView = notView.add(btnRemove).add(btnEdit); edit = edit.add(btnRemove).add(btnEdit); search = search.add(btnRemove).add(btnEdit);
					btnRemove.click(function () {
						individual[property_uri] = individual[property_uri].filter(function (_, j) {return j !== i; });
					});
					btnEdit.click(function () {
						individual[property_uri] = individual[property_uri].filter(function (_, j) {return j !== i; });
					});
					valueHolder.after( btnEdit, btnRemove );
					
					return result;
				});
			}
			
			function propertyModifiedHandler(doc_property_uri) {
				if (doc_property_uri === property_uri) {
					rendered.map(function (item) { item.remove() });
					rendered = renderPropertyValues();
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
			
		});
		
		// Property control
		$("veda-control[property]", template).map( function () {
			var control = $(this),
				property_uri = control.attr("property"),
				property = veda.ontology[property_uri],
				type = control.attr("type") || veda.ontology[property_uri]["rdfs:range"][0].id,
				spec = specs[property_uri],
				controlType;
			
			switch (type) {
				case "rdfs:Literal": 
				case "xsd:string": 
					controlType = $.fn.veda_string;
					break;
				case "xsd:boolean": 
					controlType = $.fn.veda_boolean; 
					break;
				case "xsd:integer": 
				case "xsd:nonNegativeInteger":
					controlType = $.fn.veda_integer; 
					break;
				case "xsd:decimal":
					controlType = $.fn.veda_decimal; 
					break;
				case "xsd:dateTime": 
					controlType = $.fn.veda_dateTime; 
					break;
				default: 
					controlType = $.fn["veda_" + type];
					break;
			}

			var opts = {
				individual: individual,
				property_uri: property_uri,
				spec: spec,
				mode: mode
			};

			if (property_uri === "v-s:script" || property_uri === "v-ui:template") {
				controlType = $.fn.veda_source;
			}

			controlType.call(control, opts);
			
			template.on("view edit search", function (e) {
				control.trigger(e.type);
				e.type === "edit" ? 
					isValid(individual, spec, individual[property_uri]) ? control.addClass("has-success").removeClass("has-error") : control.addClass("has-error").removeClass("has-success") 
					:
					control.removeClass("has-error has-success");
				e.stopPropagation();
			});
			
			function propertyModifiedHandler(doc_property_uri) {
				if (doc_property_uri === property_uri && mode === "edit") {
					// Check values validity
					isValid(individual, specs[property_uri], individual[property_uri]) ? control.addClass("has-success").removeClass("has-error") : control.addClass("has-error").removeClass("has-success");
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
			
			function assignDefaultValue (e) {
				var defaultValue;
				switch (property["rdfs:range"][0].id) {
					case "xsd:boolean": 
						defaultValue = spec && spec.hasValue("v-ui:defaultBooleanValue") ? spec["v-ui:defaultBooleanValue"][0] : undefined;
						break;
					case "xsd:integer": 
					case "xsd:nonNegativeInteger":
						defaultValue = spec && spec.hasValue("v-ui:defaultIntegerValue") ? spec["v-ui:defaultIntegerValue"][0] : undefined; 
						break;
					case "xsd:decimal":
						defaultValue = spec && spec.hasValue("v-ui:defaultDecimalValue") ? spec["v-ui:defaultDecimalValue"][0] : undefined; 
						break;
					case "xsd:dateTime": 
						defaultValue = spec && spec.hasValue("v-ui:defaultDatetimeValue") ? spec["v-ui:defaultDatetimeValue"][0] : undefined;
						break;
					default: 
						defaultValue = spec && spec.hasValue("v-ui:defaultStringValue") ? spec["v-ui:defaultStringValue"][0] : undefined;
						break;
				}
				
				if (defaultValue) individual[property_uri] = [ defaultValue ];
				return false;
			}
			if ( spec && !individual.hasValue(property_uri) ) {
				template.on("edit", assignDefaultValue);
				if ( mode === "edit" ) assignDefaultValue();
			}
		});
		
		// Relation control
		$("veda-control[rel]", template).map( function () {
			
			var control = $(this), 
				rel_uri = control.attr("rel"),
				spec = specs[rel_uri],
				rel = veda.ontology[rel_uri];
				
			if ( !individual[rel_uri] ) individual.defineProperty(rel_uri);
			
			var opts = {
				limit: 100,
				select: function (selected) {
					individual[rel_uri] = individual[rel_uri].concat(selected);
				},
				queryPrefix: spec && spec.hasValue("v-ui:queryPrefix") ? spec["v-ui:queryPrefix"][0] : undefined,
				add: function () {
					var newVal = new veda.IndividualModel();
					newVal["rdf:type"] = veda.ontology[rel_uri]["rdfs:range"];
					individual[rel_uri] = individual[rel_uri].concat(newVal);
				}
			};
			
			control.veda_link(opts);

			function modeHandler() {
				isValid(individual, spec, individual[rel_uri]) ? control.addClass("has-success").removeClass("has-error") : control.addClass("has-error").removeClass("has-success");
				return false;
			}
			template.on("edit", modeHandler);
			
			function propertyModifiedHandler(doc_rel_uri) {
				if (doc_rel_uri === rel_uri && mode === "edit") {
					isValid(individual, spec, individual[rel_uri]) ? control.addClass("has-success").removeClass("has-error") : control.addClass("has-error").removeClass("has-success");
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
			
			function assignDefaultObjectValue (e) {
				individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
				return false;
			}
			if ( spec && spec.hasValue("v-ui:defaultObjectValue") && !individual.hasValue(rel_uri) ) {
				template.on("edit", assignDefaultObjectValue);
				if (mode === "edit") individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
			}
			
			// tooltip from spec
			if (spec && spec.hasValue("v-ui:tooltip")) {
				control.tooltip({
					title: spec["v-ui:tooltip"].join(", "),
					placement: "top",
					container: control,
					trigger: "focus"
				});
			}
			
		});

		return template;
	}
	
	function renderRelationValues (individual, rel_uri, relContainer, relTemplate, isEmbedded, spec, embedded, template, mode) {
		if ( !individual[rel_uri] ) individual.defineProperty(rel_uri);
		var values = individual[rel_uri];
		var renderedValues = individual[rel_uri].map( function (value) { 
			// Create the same tag container to preserve element layout
			var clone = relContainer.clone(),
				valTemplate;
			setTimeout( function () {
				if (isEmbedded) {
					if (relTemplate) {
						valTemplate = relTemplate.clone();
						value.present(clone, valTemplate, mode);
					} else {
						value.present(clone, undefined, mode);
						valTemplate = clone.children();
					}
					embedded.push(valTemplate);
				} else {
					if (relTemplate) {
						valTemplate = relTemplate.clone();
						value.present(clone, valTemplate);
					} else {
						value.present(clone);
						valTemplate = clone.children();
					}
				}
				
				clone.attr("style", "position:relative;");
				
				var clear;
				if (valTemplate.prop("tagName") === "SPAN") {
					clear = $( $("#link-clear-inline-button-template").html() );
				} else {
					clear = $( $("#link-clear-block-button-template").html() );
				}
									
				clear.on("click", function () {
					clone.remove();
					individual[rel_uri] = individual[rel_uri].filter(function (item) { return item.id != value.id; });
					if (embedded.length) {
						var index = embedded.indexOf(valTemplate);
						if ( index >= 0 ) embedded.splice(index, 1);
					}
				});

				mode === "view" ? clear.hide() :
				mode === "edit" ? clear.show() :
				mode === "search" ? clear.show() : true;
				
				function modeHandler (e) {
					e.type === "view" ? clear.hide() :
					e.type === "edit" ? clear.show() :
					e.type === "search" ? clear.show() :
					true;
					e.stopPropagation();						
				}
				template.on("view edit search", modeHandler);

				clone.append(clear);
				
			}, 0);
			relContainer.before(clone);
			return clone.show();
		});
		return renderedValues;
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
				
				var range = property["rdfs:range"] ? property["rdfs:range"][0].id : "rdfs:Literal";
				switch( range ) {
					case "rdfs:Literal" : 
					case "xsd:string" : 
					case "xsd:boolean" : 
					case "xsd:nonNegativeInteger" : 
					case "xsd:integer" : 
					case "xsd:decimal" : 
					case "xsd:dateTime" :
						$(".value", result).append (
							$("<div/>").attr("property", property_uri),
							$("<veda-control class='-view edit search'></veda-control>").attr("property", property_uri).attr("type", range)
						);
					break;
					default:
						$(".value", result).append (
							$("<div/>", {"rel": property_uri, "template": "v-ui:ClassNameLabelTemplate"}),
							$("<veda-control class='-view edit search'></veda-control>").attr("rel", property_uri)
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
