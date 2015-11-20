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
		if (container.prop("id") === "main" && location.hash.indexOf(individual.id) < 0) {
			var hash = ["#", individual.id].join("/");
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
			//template = template.first();
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
					var presenter = new Function("veda", "individual", "container", "template", "mode", script + "//# sourceURL=" + individual["rdf:type"][0].id + "Presenter.js");
					presenter(veda, individual, container, view.template, mode);
				});
			}, 0);
		});
	});
	
	function renderTemplate (individual, container, template, specs, mode) {

		// Unwrapped templates support
		var wrapper = $("<div>").append(template);

		// Additional buttons change for drafts 
		if (individual.is('v-s:DraftAllowed')) {
			// If individual is draft
			if (individual.hasValue('v-s:isDraftOf')) {
				//TODO Put link to actual version
				//Rename edit button
				$('#edit', wrapper).attr('about', 'v-b:ContinueEdit');
				
				//Rename delete button
				$('#delete', wrapper).attr('about', 'v-b:DeleteDraft');
				
				//Hide send button
				$('#send', wrapper).remove();

			} else if (individual.hasValue('v-s:hasDraft')) {			
				//TODO Put link to draft version
				
				//Rename edit button
				$('#edit', wrapper).attr('about', 'v-b:ContinueEdit');
								
				//Hide send button
				$('#send', wrapper).remove();
			} 
		}
		if (container.prop("id") === "main" && individual.is('v-s:Versioned') && individual.hasValue('v-s:actualVersion') && individual['v-s:actualVersion'][0].id !== individual.id) {
			var versionBundle = new veda.IndividualModel('v-b:DocumentIsVersion');
			var actualVersionClass = new veda.IndividualModel('v-s:actualVersion');
			var previousVersionClass = new veda.IndividualModel('v-s:previousVersion');
			var nextVersionClass = new veda.IndividualModel('v-s:nextVersion');
			var $versionToolbar = $('<div />', {
	   			   "class" : "lert alert-warning no-margin",
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
		function syncEmbedded (e) {
			embedded.map(function (item) {
				item.trigger(e.type);
			});
			e.stopPropagation();
		}
		template.on("view edit search save cancel delete recover showRights", syncEmbedded);
				
		// Define handlers
		function saveHandler (e) {
			individual.save();
			template.trigger("view");
			// Change location.hash if individual was presented in #main container
			if (container.prop("id") === "main") {
				var hash = ["#", individual.id].join("/");
				if (hash !== location.hash) riot.route(hash, false);
			}
			e.stopPropagation();
		}
		template.on("save", saveHandler);
		
		function showRightsHandler (e) {
			individual.trigger("showRights");
		}
		template.on("showRights", showRightsHandler);

		function cancelHandler (e) {
			template.trigger("view");
			individual.reset();
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
		var $edit = $("#edit.action", wrapper),
			$save = $("#save.action", wrapper),
			$showRights = $("#rightsOrigin.action", wrapper),
			$cancel = $("#cancel.action", wrapper),
			$delete = $("#delete.action", wrapper);

		// Check rights to manage buttons		
		// Update
		if ( individual.isSync() ) {
			if ($edit.length   && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $edit.remove();
			if ($save.length   && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $save.remove();
			if ($cancel.length && !(individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ) $cancel.remove();
			// Delete
			if ($delete.length && !(individual.rights.hasValue("v-s:canDelete") && individual.rights["v-s:canDelete"][0] == true) ) $delete.remove();
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
			mode === "edit" && (individual.rights.hasValue("v-s:canUpdate") && individual.rights["v-s:canUpdate"][0] == true) ? template.addClass("mode-edit").removeClass("mode-view mode-search") :
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
					renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls);
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
			
			template.on("edit", function () {
				if ( !individual.hasValue(rel_uri) && isEmbedded/* && spec && spec["v-ui:minCardinality"][0] >= 1*/ ) {
					var emptyValue = new veda.IndividualModel();
					if ( individual.properties[rel_uri].hasValue("rdfs:range") ) {
						emptyValue["rdf:type"] = individual.properties[rel_uri]["rdfs:range"].slice(0);
					}
					individual[rel_uri] = [emptyValue];
				}
			});

			var values = about[rel_uri], rendered = {}, counter = 0;
			
			relContainer.empty();
			
			propertyModifiedHandler(rel_uri, values);

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
								var renderedTmpl = renderRelationValue (about, rel_uri, value, relContainer, relTemplate, isEmbedded, embedded, mode, isAbout);
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
			about.on("individual:propertyModified", propertyModifiedHandler);
			template.one("remove", function () {
				about.off("individual:propertyModified", propertyModifiedHandler);
			});
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
				property = veda.ontology[property_uri],
				type = control.attr("type") || veda.ontology[property_uri]["rdfs:range"][0].id,
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

			function assignDefaultValue () {
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
				return false;
			}
			template.on("edit", assignDefaultValue);
			
		});
		
		// Relation control
		$("veda-control[rel]:not([rel] *):not([about] *)", wrapper).map( function () {
			
			var control = $(this), 
				rel_uri = control.attr("rel"),
				spec = specs[rel_uri],
				rel = veda.ontology[rel_uri],
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

			function assignDefaultObjectValue () {
				if ( spec && spec.hasValue("v-ui:defaultObjectValue") && !individual.hasValue(rel_uri) ) {
					individual[rel_uri] = [ spec["v-ui:defaultObjectValue"][0] ];
				}
				return false;
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
		
		// add icon to label
		if (individual.is('v-s:DraftAllowed') && individual.hasValue('v-s:isDraftOf')) {			
			//Put draft sign
			$('[about="'+individual.id+'"][property="rdfs:label"]', wrapper).append('<span class="glyphicon glyphicon-pencil draftsign" style="font-size: 10px;">');
		} else if (individual.is('v-s:Versioned') && individual.hasValue('v-s:actualVersion') && individual['v-s:actualVersion'][0].id!=individual.id) {
			//Put version sign
			$('[about="'+individual.id+'"][property="rdfs:label"]', wrapper).append('<span class="glyphicon glyphicon-camera draftsign" style="font-size: 10px;">');
		}
		
		if (individual.is('v-s:DraftAllowed')) {
			var $draft = $("#draft.action", wrapper);			 
			$draft.unbind("click");
			$draft.on("click", function () {
				if (!individual.hasValue('v-s:isDraftOf')) {
					// If `v-s:isDraftOf` is empty, then current individual is "draftonly" individual
					individual['v-s:isDraftOf'] = [individual];
				}
				individual.save();
				container.empty();
				individual.present(container, undefined, "view");
				changeHash(individual.id);
			});
			
			// Remove edit button if no rights to edit draft
			if (individual.hasValue('v-s:hasDraft')) {
				var draft = new veda.IndividualModel(individual['v-s:hasDraft'][0].id);
				if ($edit.length   && !(draft.rights.hasValue("v-s:canUpdate") && draft.rights["v-s:canUpdate"][0] == true) ) $edit.remove();
			}
			
			// Remove delete button if document has draft
			if (individual.hasValue('v-s:hasDraft')) {
				$delete.remove();
			}
			
			$edit.unbind("click");
			$edit.on("click", function (e) {
				// Go to edit draft instead of edit individual
				if (individual.hasValue('v-s:hasDraft')) {
					// Go to already existed draft
					var draft = new veda.IndividualModel(individual['v-s:hasDraft'][0].id);
					container.empty();
					draft.present(container, undefined, "edit");
					changeHash(draft.id, "edit");
				} else 
				{
					if (individual.hasValue('v-s:isDraftOf')) {
						// Individual is draft itself
						container.empty();
						individual.present(container, undefined, "edit");
						changeHash(individual.id, "edit");
					} else {
						// Create new draft
						var clone = individual.clone();
						clone['v-s:hasDraft'] = [];
						clone['v-s:isDraftOf'] = [individual];
						clone.save();
	
						// Add link to draft
						individual['v-s:hasDraft'] = [clone];
						individual.save();
						
						container.empty();
						clone.present(container, undefined, "edit");
						changeHash(clone.id, "edit");
					}
				}
			});
			
			if (individual.hasValue('v-s:isDraftOf') || individual.is('v-s:Versioned')) 
			{
				$save.unbind("click");
				$save.on("click", function (e) {
					// Before
					var previousId = individual.hasValue('v-s:isDraftOf')?
											(individual['v-s:isDraftOf'][0].hasValue('v-s:previousVersion')?
											 individual['v-s:isDraftOf'][0]['v-s:previousVersion'][0].id:
											 null):
											(individual.hasValue('v-s:previousVersion')?
											 individual['v-s:previousVersion'][0].id:
											 null);
					var actualId = individual.hasValue('v-s:isDraftOf')?individual['v-s:isDraftOf'][0].id:individual.id;
					var versionId = (actualId==individual.id)?veda.Util.genUri():individual.id;
					console.log('previousId > '+previousId);
					
					// After
					var actual = individual.clone();						
					var version = individual.clone();
					var previous = previousId!=null?new veda.IndividualModel(previousId):null;
					actual.id = actualId;
					version.id = versionId;
					
					// Save draft as actual version
					actual['v-s:isDraftOf'] = [];
					actual['v-s:hasDraft'] = [];
					actual['v-s:previousVersion'] = [version];
					actual['v-s:actualVersion'] = [actual];
					actual['v-s:nextVersion'] = [];
					actual.save();
						
					// Save draft as old version
					version['v-s:isDraftOf'] = [];
					version['v-s:hasDraft'] = [];
					version['v-s:previousVersion'] = [previous];
					version['v-s:actualVersion'] = [actual];
					version['v-s:nextVersion'] = [actual];
					version.save();
					
					// Update draft version
					if (previous!=null) 
					{
						previous['v-s:nextVersion'] = [version];
						previous.save();
					}
					
					container.empty();
					actual.present(container, undefined, "view");
					changeHash(actualId);
				});			
			}
			$delete.unbind("click");
			$delete.on("click", function (e) {
				if (individual.hasValue('v-s:isDraftOf')) {
					individual.delete();
					var original = new veda.IndividualModel(individual['v-s:isDraftOf'][0].id);
					container.empty();
					original.present(container, undefined, "view");
					changeHash(original.id);
				} else {
					individual.delete();
					container.empty();
					individual.present(container, undefined, "view");
				}
			});
		}

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

	function renderPropertyValues(individual, property_uri, propertyContainer, props_ctrls) {
		propertyContainer.empty();
		individual[property_uri].map( function (value, i) {
			var valueHolder = $("<span class='value-holder'/>");
			propertyContainer.append(valueHolder.text( formatValue(value) ));
			var wrapper = $("<div id='prop-actions' class='btn-group btn-group-xs -view edit search' role='group'></div>");
			var btnEdit = $("<button class='btn btn-default'><span class='glyphicon glyphicon-pencil'></span></button>");
			var btnRemove = $("<button class='btn btn-default'><span class='glyphicon glyphicon-remove'></span></button>");
			wrapper.append(btnEdit, btnRemove);
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
	
	function renderRelationValue(individual, rel_uri, value, relContainer, relTemplate, isEmbedded, embedded, mode, isAbout) {
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
			valTemplate.css("position", "relative");
			// It is important to append buttons skipping script element in template!
			valTemplate.not("script").append(wrapper);
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
