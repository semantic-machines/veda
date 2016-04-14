// Individual Presenter

veda.Module(function IndividualPresenter(veda) { "use strict";
	
	var deletedAlertTmpl = $("#deleted-individual-alert-template").html();
	
	//var c = 0; 
	
	veda.on("individual:loaded", function (individual, container, template, mode) {
		
		//console.log(individual.id, "presenter count:", ++c);
		
		if (typeof container === "string") { 
			container = $(container).empty();
		}
		mode = mode || "view";
		
		// Change location.hash if individual was presented in #main container
		/*if (container.prop("id") === "main") {
			$("#current-individual").text(individual["rdfs:label"].join(", "));
			if (location.hash.indexOf(individual.id) < 0) {
				var hash = ["#", individual.id].join("/");
				if (hash !== location.hash) riot.route(hash, false);
			}
		}*/

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
			if (typeof template === "string") {
				if (template === "generic") {
					var _class = individual.hasValue("rdf:type") ? individual["rdf:type"][0] : undefined ;
					template = genericTemplate(individual, _class);
				} else if (template === "json") {
					var pre = $("<pre>"), 
						json = JSON.parse(individual._.original_individual),
						ordered = {};
					Object.keys(json).sort().forEach(function(key) {
						ordered[key] = json[key];
					});
					json = JSON.stringify(ordered, null, 2);
					pre.text(json);
					container.html(pre);
					return;
				} else {
					template = $( template );
				}
			}
			var $scripts = template.filter("script");
			$scripts.map(function () { scripts.push( $(this).text() ); });
			template = template.filter("*:not(script)");
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
						//template = template.first();
						template = template.filter("*:not(script)");
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
			view.template
				.attr("resource", individual.id)
				.attr("typeof", individual["rdf:type"].map(function (item) { return item.id; }).join(" ") )
				.addClass("mode-" + mode);
			container.append(view.template);
			individual.trigger("individual:templateReady", view.template);
			// Timeout to wait all related individuals to render
			setTimeout(function () {
				view.template.trigger(mode);
				view.scripts.map( function (script) { 
					var presenter = new Function("veda", "individual", "container", "template", "mode", script + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js" /*+ "." + veda.Util.guid()*/ );
					presenter(veda, individual, container, view.template, mode);
				});
			}, 0);
		});
	});
	
	function renderTemplate (individual, container, template, specs, mode) {

		// Unwrapped templates support
		var wrapper = $("<div>").append(template);
		
		var view = $(".view", wrapper);
		var edit = $(".edit", wrapper);
		var search = $(".search", wrapper);
		var _view = $(".-view", wrapper);
		var _edit = $(".-edit", wrapper);
		var _search = $(".-search", wrapper);
		function showHideHandler (e) {
			switch (e.type) {
				case "view": view.show(); _view.hide(); break;
				case "edit": edit.show(); _edit.hide(); break;
				case "search": search.show(); _search.hide(); break;
			}
			e.stopPropagation();
		}
		template.on("view edit search", showHideHandler);

		if (container.prop("id") === "main" 
			&& individual.is('v-s:Versioned') 
			&& individual.hasValue('v-s:actualVersion') 
			&& individual['v-s:actualVersion'][0].id !== individual.id 
		) {
			var versionBundle = new veda.IndividualModel('v-s:DocumentIsVersion');
			var actualVersionClass = new veda.IndividualModel('v-s:actualVersion');
			var previousVersionClass = new veda.IndividualModel('v-s:previousVersion');
			var nextVersionClass = new veda.IndividualModel('v-s:nextVersion');
			var $versionToolbar = $('<div />', {
	   			   "class" : "alert alert-warning margin-sm",
	   			   "style" : "padding:5px;",
	   			   "role" : "alert",
         		   "html" : ('<div>'+versionBundle['rdfs:label']+'</div>')
         		   		   +('<div>'+actualVersionClass['rdfs:label']+' : <a href="/#/'+individual['v-s:actualVersion'][0].id+'">'+individual['v-s:actualVersion'][0]['rdfs:label'][0]+'</a></div>')
	   		   			   +(individual.hasValue('v-s:previousVersion')?('<div>'+previousVersionClass['rdfs:label']+' : <a href="/#/'+individual['v-s:previousVersion'][0].id+'">'+individual['v-s:previousVersion'][0]['rdfs:label'][0]+'</a></div>'):'')
		   		           +((individual.hasValue('v-s:nextVersion') && individual['v-s:nextVersion'][0].id !== individual['v-s:actualVersion'][0].id)?('<div>'+nextVersionClass['rdfs:label']+' : <a href="/#/'+individual['v-s:nextVersion'][0].id+'">'+individual['v-s:nextVersion'][0]['rdfs:label'][0]+'</a></div>'):'')
         	   });
			$('#main').prepend($versionToolbar);
		}
		
		// Cleanup memory
		/*template.on("remove", function (event) {
			$(".typeahead", wrapper).typeahead("destroy");
			wrapper = embedded = props_ctrls = null;
		});*/
		
		// Embedded templates list & property controls
		var embedded = [];

		// Trigger same events for embedded templates
		function syncEmbedded (e, parent) {
			embedded.map(function (item) {
				item.trigger(e.type, individual.id);
			});
			e.stopPropagation();
		}
		template.on("view edit search save cancel delete recover draft", syncEmbedded);
				
		// Define handlers
		function saveHandler (e, parent) {
			individual.save(parent);
			template.trigger("view");
			e.stopPropagation();
		}
		template.on("save", saveHandler);
				
		function draftHandler (e, parent) {
			individual.draft(parent);
			template.trigger("view");
			e.stopPropagation();
		}
		template.on("draft", draftHandler);
		
		function showRightsHandler (e) {
			individual.trigger("showRights");
			e.stopPropagation();
		}
		template.on("showRights", showRightsHandler);

		function cancelHandler (e) {
			individual.reset();
			template.trigger("view");
			e.stopPropagation();
		}
		template.on("cancel", cancelHandler);

		// Deleted alert
		var deletedAlert = $( deletedAlertTmpl );
		var recoverBtn = $("button", deletedAlert);
		if (individual.hasValue("v-s:deleted") && individual["v-s:deleted"][0] == true) {
			afterDeleteHandler();
		}
		function afterRecoverHandler() {
			deletedAlert.remove();
		}
		function afterDeleteHandler() {
			template.prepend(deletedAlert);
			recoverBtn.click(function () {
				template.trigger("recover");
			});
		}
		individual.on("individual:afterRecover", afterRecoverHandler);
		individual.on("individual:afterDelete", afterDeleteHandler);
		template.one("remove", function () {
			individual.off("individual:afterRecover", afterRecoverHandler);
			individual.off("individual:afterDelete", afterDeleteHandler);
		});

		function deleteHandler (e, parent) {
			individual.delete(parent);
			e.stopPropagation();
		}
		template.on("delete", deleteHandler);

		function recoverHandler (e, parent) {
			individual.recover(parent);
			e.stopPropagation();
		}
		template.on("recover", recoverHandler);
		
		// Actions
		var $edit = $("#edit.action", wrapper),
			$save = $("#save.action", wrapper),
			$draft = $("#draft.action", wrapper),
			$showRights = $("#rightsOrigin.action", wrapper),
			$cancel = $("#cancel.action", wrapper),
			$delete = $("#delete.action", wrapper);

		// Check rights to manage buttons		
		// Update
		if ( individual.isSync() ) {
			if ($edit.length   && !(individual.rights && individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $edit.remove();
			if ($save.length   && !(individual.rights && individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $save.remove();
			if ($draft.length  && !(individual.rights && individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $draft.remove();
			if ($cancel.length && !(individual.rights && individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $cancel.remove();
			// Delete
			if ($delete.length && !(individual.rights && individual.rights.hasValue("v-s:canDelete") && individual.rights["v-s:canDelete"][0] == true) ) $delete.remove();
		}
		
		// Buttons handlers
		// Edit
		$edit.on("click", function (e) {
			template.trigger("edit");
		});
		
		// Save
		$save.on("click", function (e) {
			template.trigger("save");
		});

		// Draft
		$draft.on("click", function (e) {
			template.trigger("draft");
		});
		
		// Show rights
		$showRights.on("click", function (e) {
			template.trigger("showRights");
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
		
		// Apply mode class to template to show/hide elements in different modes
		function modeHandler (e) {
			mode = e.type;
			mode === "view" ? template.addClass("mode-view").removeClass("mode-edit mode-search") :
			mode === "edit" && (individual.rights && individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ? template.addClass("mode-edit").removeClass("mode-view mode-search") :
			mode === "search" ? template.addClass("mode-search").removeClass("mode-view mode-edit") : 
			true;
			e.stopPropagation();
		}
		template.on("view edit search", modeHandler);

		// Validation with support of embedded templates (arbitrary depth)
		function validationHandler () {
			var isValid = checkState(template);			
			isValid = isValid && embedded.reduce(function (state, template) {
				return state && template.data("valid").state;
			}, true);
			template.data("valid").state = isValid;
			if (isValid) { 
				$save.removeAttr("disabled");
				template.trigger("valid");
			} else {
				$save.attr("disabled", "disabled");
				template.trigger("invalid");
			}
			// "validate" event bubbles up to be handled by parent templates
		}
		template.on("validate", validationHandler);
		// Initial validation state
		template.data("valid", {state: true});

		// Process RDFa compliant template

		// Special (not RDFa)
		$("[href*='@']:not([rel] *):not([about] *)", wrapper).map( function () {
			var self = $(this);
			var str = self.attr("href");
			self.attr("href", str.replace("@", individual.id));
		});

		$("[src*='@']:not([rel] *):not([about] *)", wrapper).map( function () {
			var self = $(this);
			var str = self.attr("src");
			self.attr("src", str.replace("@", individual.id));
		});

		// Property value
		var props_ctrls = {};
		$("[property]:not(veda-control):not([rel] *):not([about]):not([about] *)", wrapper).map( function () {
			var propertyContainer = $(this),
				property_uri = propertyContainer.attr("property"),
				spec = specs[property_uri];
			if (property_uri === "@") { 
				propertyContainer.text(individual.id);
				return;
			}
			if (!individual[property_uri]) {
				individual.defineProperty(property_uri);
			}
			propertyModifiedHandler(property_uri);
			// Re-render all property values at propertyModified event from model
			function propertyModifiedHandler(doc_property_uri) {
				if (doc_property_uri === property_uri) {
					renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls, template, mode);
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});
		});

		// Related resources & about resources
		$("[rel]:not(veda-control):not([rel] *):not([about] *)", wrapper).map( function () {
			var relContainer = $(this), 
				about = relContainer.attr("about"),
				rel_uri = relContainer.attr("rel"),
				isEmbedded = relContainer.attr("embedded") === "true",
				spec = specs[rel_uri],
				rel_inline_template = relContainer.children(),
				rel_template_uri = relContainer.attr("template"),
				relTemplate,
				isAbout;
			
			if (about) {
				isAbout = true;
				about = (about === "@" ? individual : new veda.IndividualModel(about));
				relContainer.attr("about", about.id);
			} else {
				isAbout = false;
				about = individual;
			}
			
			if ( rel_template_uri ) {
				var templateIndividual = new veda.IndividualModel( rel_template_uri );
				relTemplate = $( templateIndividual["v-ui:template"][0].toString() );
			}
			if ( rel_inline_template.length ) {
				relTemplate = rel_inline_template.remove();
			}
			rel_inline_template = null;
			if ( !about[rel_uri] ) {
				about.defineProperty(rel_uri);
			}
			
			template.on("edit", function (e) {
				if ( isEmbedded 
					 && spec 
					 && spec["v-ui:minCardinality"][0] >= 1 
					 && !individual.hasValue(rel_uri)  
					 && !(individual.properties[rel_uri].hasValue("rdfs:range") && individual.properties[rel_uri]["rdfs:range"][0].id === "v-s:File")
				) {
					var emptyValue = new veda.IndividualModel();
					if ( individual.properties[rel_uri].hasValue("rdfs:range") ) {
						emptyValue["rdf:type"] = individual.properties[rel_uri]["rdfs:range"].slice(0);
					}
					individual[rel_uri] = [emptyValue];
				}
				e.stopPropagation();
			});

			var values = about[rel_uri], rendered = {}, counter = 0;
			
			relContainer.empty();
			
			propertyModifiedHandler(rel_uri, values);
			about.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				about.off("individual:propertyModified", propertyModifiedHandler);
			});

			if (isEmbedded) {
				embeddedHandler(rel_uri, values);
				about.on("individual:propertyModified", embeddedHandler);
				template.one("remove", function () {
					about.off("individual:propertyModified", embeddedHandler);
				});
			}

			// Re-render link property if its' values were changed
			function propertyModifiedHandler (doc_rel_uri, values) {
				if (doc_rel_uri === rel_uri) {
					++counter;
					if (values.length) {
						values.map(function (value) {
							if (value.id in rendered) {
								rendered[value.id].cnt = counter;
								return;
							}
							setTimeout (function () {
								var renderedTmpl = renderRelationValue (about, rel_uri, value, relContainer, relTemplate, isEmbedded, embedded, isAbout, template, mode);
								rendered[value.id] = {tmpl: renderedTmpl, cnt: counter};
							}, 0);
						});
					} else {
						relContainer.empty();
					}
					// Remove rendered templates for removed values
					for (var i in rendered) {
						if (rendered[i].cnt === counter) continue; 
						rendered[i].tmpl.remove();
						delete rendered[i];
					}
				}
			}
			
			function embeddedHandler(doc_rel_uri, values) {
				if (doc_rel_uri === rel_uri) {
					values.map(function (value) {
						if ( !value["v-s:parent"] ) { value.defineProperty("v-s:parent"); }
						value["v-s:parent"] = [about];
					});
				}
			}
			
		});		

		// About resource
		$("[about]:not([rel]):not([property])", wrapper).map( function () {
			var aboutContainer = $(this), 
				about_template_uri = aboutContainer.attr("template"),
				about_inline_template = aboutContainer.children(),
				about, aboutTemplate;
			if ( about_template_uri ) {
				var templateIndividual = new veda.IndividualModel( about_template_uri );
				aboutTemplate = $( templateIndividual["v-ui:template"][0].toString() );
			}
			if ( about_inline_template.length ) {
				aboutTemplate = about_inline_template.remove();
			}
			if (aboutContainer.attr("about") === "@") {
				about = individual;
				aboutContainer.attr("about", about.id);
			} else {
				about = new veda.IndividualModel(aboutContainer.attr("about"));
			}
			aboutContainer.empty();
			about.present(aboutContainer, aboutTemplate);
		});

		// About resource property
		$("[about][property]:not([rel] *):not([about] *)", wrapper).map( function () {
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
					if (property_uri === "@") {
						propertyContainer.text( about.id );
					} else if (about[property_uri] !== undefined) {
						var formatted = about[property_uri].map(formatValue).join(" ");
						propertyContainer.text( formatted );
					}
				}
			}
			about.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				about.off("individual:propertyModified", propertyModifiedHandler);
			});
		});

		// Property control
		$("veda-control[property]:not([rel] *):not([about] *)", wrapper).map( function () {
			
			var control = $(this),
				property_uri = control.attr("property"),
				property = new veda.IndividualModel(property_uri),
				type = control.attr("type") || property["rdfs:range"][0].id,
				spec = specs[property_uri],
				controlType;
			
			if ( !individual[property_uri] ) { 
				individual.defineProperty(property_uri);
			}

			control.removeAttr("property");
			
			switch (type) {
				case "rdfs:Literal": 
				case "xsd:string": 
					controlType = $.fn.veda_multilingualString;
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
			
			props_ctrls[property_uri] ? props_ctrls[property_uri].push(control) : props_ctrls[property_uri] = [ control ];
			
			var state = true;
			
			template.on("view edit search", function (e) {
				e.stopPropagation();
				control.trigger(e.type);
				if (spec) state = validate(template, spec, individual[property_uri], property_uri);
				e.type === "edit" ? 
					state ? control.removeClass("has-error") : control.addClass("has-error") 
					:
					control.removeClass("has-error");
			});
			
			function propertyModifiedHandler(doc_property_uri) {
				if (doc_property_uri === property_uri) {
					if (spec) state = validate(template, spec, individual[property_uri], property_uri);
					if (mode === "edit") {
						state ? control.removeClass("has-error") : control.addClass("has-error");
					}
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});

			function assignDefaultValue (e) {
				if ( spec && !individual.hasValue(property_uri) ) {
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
				}
				e.stopPropagation();
			}
			template.on("edit", assignDefaultValue);
			
		});
		
		// Relation control
		$("veda-control[rel]:not([rel] *):not([about] *)", wrapper).map( function () {
			
			var control = $(this), 
				rel_uri = control.attr("rel"),
				spec = specs[rel_uri],
				rel = new veda.IndividualModel(rel_uri),
				controlType = control.attr("type") ? $.fn["veda_" + control.attr("type")] : $.fn.veda_link;
			
			control.removeAttr("rel");
				
			if ( !individual[rel_uri] ) { 
				individual.defineProperty(rel_uri);
			}
			
			var opts = {
				individual: individual,
				rel_uri: rel_uri,
				spec: spec,
				mode: mode
			};
			
			controlType.call(control, opts);

			var state = true;
			
			function modeHandler(e) {
				if (spec) state = validate(template, spec, individual[rel_uri], rel_uri);
				e.stopPropagation();
				e.type === "edit" ? 
					state ? control.removeClass("has-error") : control.addClass("has-error") 
					:
					control.removeClass("has-error");
				control.trigger(e.type);
			}
			template.on("view edit search", modeHandler);
			
			function propertyModifiedHandler(doc_rel_uri) {
				if (doc_rel_uri === rel_uri) {
					if (spec) state = validate(template, spec, individual[rel_uri], rel_uri);
					if (mode === "edit") {
						state ? control.removeClass("has-error") : control.addClass("has-error");
					}
				}
			}
			individual.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				individual.off("individual:propertyModified", propertyModifiedHandler);
			});

			function assignDefaultObjectValue (e) {
				if ( spec && spec.hasValue("v-ui:defaultObjectValue") && !individual.hasValue(rel_uri) ) {
					individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
				}
				e.stopPropagation();
			}
			template.on("edit", assignDefaultObjectValue);
			
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
		
		// Standart buttons labels change for drafts 
		var Edit = (new veda.IndividualModel("v-s:Edit"))["rdfs:label"].join(" ");
		var ContinueEdit = (new veda.IndividualModel("v-s:ContinueEdit"))["rdfs:label"].join(" ");
		var DeleteDraft = (new veda.IndividualModel("v-s:DeleteDraft"))["rdfs:label"].join(" ");
		var Cancel = (new veda.IndividualModel("v-s:Cancel"))["rdfs:label"].join(" ");

		individual.on("individual:propertyModified", isDraftHandler);
		template.on("remove", function () {
			individual.off("individual:propertyModified", isDraftHandler);
		});
		function isDraftHandler(property_uri) {
			if (property_uri === "v-s:isDraft") {
				// If individual is draft
				if ( individual.hasValue("v-s:isDraft") && individual["v-s:isDraft"][0] == true ) {
					//Rename "Edit" -> "Continue edit"
					$edit.text(ContinueEdit);
					//Rename "Cancel" -> "Delete draft"
					$cancel.text(DeleteDraft);
				} else {
					//Rename "Continue edit" -> Edit"
					$edit.text(Edit);
					//Rename "Delete draft" -> "Cancel" 
					$cancel.text(Cancel);
				}
			}
		}
		isDraftHandler("v-s:isDraft");
		
		// standard tasks
		$('#standard-tasks', template).each(function() {
			var stask = $(this);
			stask.append($('<li/>', {
				style:'cursor:pointer', 
				click: function() {veda.Util.send(individual, template, 'v-wf:questionRouteStartForm')},
				html: '<a>'+(new veda.IndividualModel('v-s:SendQuestion')['rdfs:label'][0])+'</a>'
			}));
			stask.append($('<li/>', {
				style:'cursor:pointer', 
				click: function() {veda.Util.send(individual, template, 'v-wf:instructionRouteStartForm')},
				html: '<a>'+(new veda.IndividualModel('v-s:SendInstruction')['rdfs:label'][0])+'</a>'
			}));
		});

		return template;
	}

	function formatValue (value) {
		var formatted;
		switch (true) {
			case value instanceof Date:
				formatted = veda.Util.formatDate(value);
				break;
			case value instanceof Number:
				formatted = veda.Util.formatNumber(value);
				break;
			default: 
				formatted = value.toString();
		}
		return formatted;
	}

	function renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls, template, mode) {
		propertyContainer.empty();
		individual[property_uri].map( function (value, i) {
			var valueHolder = $("<span class='value-holder'/>");
			propertyContainer.append(valueHolder.text( formatValue(value) ));
			var wrapper = $("<div id='prop-actions' class='btn-group btn-group-xs' role='group'></div>");
			var btnEdit = $("<button class='btn btn-default'><span class='glyphicon glyphicon-pencil'></span></button>");
			var btnRemove = $("<button class='btn btn-default'><span class='glyphicon glyphicon-remove'></span></button>");
			wrapper.append(btnEdit, btnRemove);
			
			template.on("view edit search", function (e) {
				if (e.type === "view") wrapper.hide();
				else wrapper.show();
				e.stopPropagation();
			});
			if (mode === "view") { wrapper.hide(); }
			
			btnRemove.click(function () {
				individual[property_uri] = individual[property_uri].filter(function (_, j) {return j !== i; });
			});
			btnRemove.mouseenter(function () {
				valueHolder.addClass("red-outline");
			});
			btnRemove.mouseleave(function () {
				valueHolder.removeClass("red-outline");
			});
			btnEdit.click(function () {
				var val;
				individual[property_uri] = individual[property_uri].filter(function (_, j) {
					var test = j !== i;
					if (!test) val = individual[property_uri][j];
					return test;
				});
				if ( props_ctrls[property_uri] ) {
					props_ctrls[property_uri].map(function (item, i) {
						item.val(val);
						if (i === 0) item.trigger("veda_focus");
					});
				}
			});
			valueHolder.after( wrapper );
		});
	}
	
	function renderRelationValue(individual, rel_uri, value, relContainer, relTemplate, isEmbedded, embedded, isAbout, template, mode) {
		var valTemplate;
		if (isEmbedded) {
			if (relTemplate) {
				valTemplate = relTemplate.clone();
				value.present(relContainer, valTemplate, mode);
			} else {
				value.present(relContainer, undefined, mode);
				valTemplate = $("[resource='" + value.id + "']", relContainer);
			}
			embedded.push(valTemplate);
			valTemplate.on("remove", function () {
				if (embedded.length) {
					var index = embedded.indexOf(valTemplate);
					if ( index >= 0 ) embedded.splice(index, 1);
				}
			});
		} else {
			if (relTemplate) {
				valTemplate = relTemplate.clone();
				value.present(relContainer, valTemplate);
			} else {
				value.present(relContainer);
				valTemplate = $("[resource='" + value.id + "']", relContainer);
			}
		}
		if (!isAbout) {
			var wrapper = $("<div id='rel-actions' class='btn-group btn-group-xs -view edit search' role='group'></div>");
			var btnRemove = $("<button class='btn btn-default button-delete'><span class='glyphicon glyphicon-remove'></span></button>");
			wrapper.append(btnRemove);
			template.on("view edit search", function (e) {
				if (e.type === "view") wrapper.hide();
				else wrapper.show();
				e.stopPropagation();
			});
			if (mode === "view") { wrapper.hide(); }

			if (valTemplate.prop("tagName") !== "SPAN") {
				wrapper.addClass("block");
			}
			if (valTemplate.attr("deleteButton") == "hide") {
				btnRemove.hide();
			}
			btnRemove.on("click", function () {
				individual[rel_uri] = individual[rel_uri].filter(function (item) { return item.id !== value.id; });
			});
			btnRemove.mouseenter(function () {
				valTemplate.addClass("red-outline");
			});
			btnRemove.mouseleave(function () {
				valTemplate.removeClass("red-outline");
			});
			if (valTemplate.prop("tagName") === "TR") {
				var td = $("td", valTemplate).last();
				td.css("position", "relative").append(wrapper);
			} else {
				valTemplate.css("position", "relative");
				// It is important to append buttons skipping script element in template!
				valTemplate.not("script").append(wrapper);
			}
		}
		return valTemplate;
	}

	// Check validity state of a template 
	function checkState (template) {
		var valid = template.data("valid");
		return Object.keys(valid).reduce(function (state, spec_id) {
			if (spec_id === "state") return state;
			var spec = valid[spec_id];
			var spec_state = Object.keys(spec).reduce(function (prop_state, property_uri) {
				return prop_state && spec[property_uri];
			}, true);
			return state && spec_state;
		}, true);
	}

	// Property validation according to specification
	function validate(template, spec, values, property_uri) {
		var valid = template.data("valid");
		valid[spec.id] = valid[spec.id] || {};
		var result = true;
		// cardinality check
		if (spec.hasValue("v-ui:minCardinality")) { 
			result = result && (
				values.length >= spec["v-ui:minCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item;}).length
			);
		}
		if (spec.hasValue("v-ui:maxCardinality")) { 
			result = result && (
				values.length <= spec["v-ui:maxCardinality"][0] && 
				// filter empty values
				values.length === values.filter(function(item){return !!item;}).length
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
		valid[spec.id][property_uri] = result;
		template.trigger("validate");
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
				var property = new veda.IndividualModel(property_uri);
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
							$("<div/>", {"rel": property_uri, "template": "v-ui:ClassNameLabelBlockTemplate"}),
							$("<veda-control class='-view edit search fullsearch fulltext'></veda-control>").attr("rel", property_uri)
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

function changeHash(individualId, mode) {
	var hash = "#/"+individualId+(mode?("///"+mode):"");
	if (hash !== location.hash) riot.route(hash, false);			
}
