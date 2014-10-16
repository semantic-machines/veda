// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";
	
	function renderEscape (value) { return value instanceof Array ? value.join(", ") : value; }
	
	function renderProperty (document, property, spec, values) {

		var result = $("<div/>");

		var propertyTemplate, renderedProperty;
		
		switch( property["rdfs:range"] ? property["rdfs:range"][0].id : "rdfs:Literal" ) {

			case "rdfs:Literal" : 
			case "xsd:string" : 
				propertyTemplate = $("#string-property-template").html();

				renderedProperty = riot.render (
					propertyTemplate, 
					{property: property},
					renderEscape
				);
				result.append(renderedProperty);

				var valueTemplate = $("#string-value-template").html();
				
				var tmp = $(".values", result);

				values.map (function (value, index) {
					var res = $("<div>");
					res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
					
					$("textarea", res).autosize();
					
					var $first = $("<li>").append( $("<a>", {href: "#", "data-language": "", text: "-"}).addClass("language") );
					if (!value.language) $first.addClass("active");
					$(".language-list", res).append(
						$first,
						Object.keys(veda.availableLanguages).map(function (language_name) {
							var $li = $("<li>"), 
								$a = $("<a>", {href: "#", "data-language": language_name, text: language_name}).addClass("language");
							$li.append($a);
							if (value.language == language_name) $li.addClass("active");
							return $li;
						})
					);
					
					$(".language", res).on("click", function ( e ) {
						e.preventDefault();
						$(".language-selector", res)
							.empty()
							.append($(this).data("language"), " <span class='caret'></span>");
						$("textarea", res)
							.data("language", $(this).data("language") )
							.trigger("change");
					});

					tmp.append(res);
				});
				
				$("[bound]", result).on("change", function ( e ) {
					document[property.id] = $("[bound]", result).map(function () {
						var res = new String(this.value);
						res.language = $(this).data("language");
						return res;
					}).get();
				});
				
				$(".remove", result).on("click", function () {
					var $target = $(this.parentNode);
					$target.remove();
					var bound = $("[bound]", result);
					if (bound.length) return bound.first().trigger("change");
					else document[property.id] = [];
				});

				$(".add", result).on("click", function () {
					var emptyVal = new String(""); emptyVal.language = undefined;
					values.push(emptyVal);
					document[property.id] = values; 
				});
				
				// Important! Return immediately
				return result; 
				break

			case "xsd:boolean" : 
				propertyTemplate = $("#property-template").html();

				renderedProperty = riot.render (
					propertyTemplate, 
					{property: property},
					renderEscape
				);
				result.append(renderedProperty);

				var valueTemplate = $("#boolean-value-template").html();
				$(".values", result).append(
					values.map (function (value, index) {
						return riot.render(valueTemplate, {value: value, index: index, property: property});
					})
				);
				$("[bound]", result).on("change", function ( e ) {
					document[property.id] = $("[bound]", result).map(function () {
						return new Boolean(this.value == "true" ? true : false);
					}).get();
				});
				break
			
			case "xsd:nonNegativeInteger" : 
			case "xsd:integer" : 
				propertyTemplate = $("#property-template").html();

				renderedProperty = riot.render (
					propertyTemplate, 
					{property: property},
					renderEscape
				);
				result.append(renderedProperty);

				var valueTemplate = $("#integer-value-template").html();
				$(".values", result).append(
					values.map (function (value, index) {
						return riot.render(valueTemplate, {value: value, index: index, property: property});
					})
				);
				$("[bound]", result).on("change", function ( e ) {
					document[property.id] = $("[bound]", result).map(function () {
						return new Number( parseInt(this.value, 10) );
					}).get();
				});
				break
			
			case "xsd:decimal" : 
				propertyTemplate = $("#property-template").html();

				renderedProperty = riot.render (
					propertyTemplate, 
					{property: property},
					renderEscape
				);
				result.append(renderedProperty);

				var valueTemplate = $("#decimal-value-template").html();
				$(".values", result).append(
					values.map (function (value, index) {
						return riot.render(valueTemplate, {value: value, index: index, property: property});
					})
				);
				$("[bound]", result).on("change", function ( e ) {
					document[property.id] = $("[bound]", result).map(function () {
						return new Number( parseFloat(this.value) );
					}).get();
				});
				break

			case "xsd:dateTime" : 
				propertyTemplate = $("#property-template").html();

				renderedProperty = riot.render (
					propertyTemplate, 
					{property: property},
					renderEscape
				);
				result.append(renderedProperty);

				var valueTemplate = $("#datetime-value-template").html();
				$(".values", result).append(
					values.map (function (value, index) {
						return riot.render(valueTemplate, {value: value, index: index, property: property});
					})
				);
				 
				$("[bound]", result).on("change", function ( e ) {
					document[property.id] = $("[bound]", result).map(function () {
						return new Date( this.value );
					}).get();
				});
				break

			default : 
				propertyTemplate = $("#property-template").html();

				renderedProperty = riot.render (
					propertyTemplate, 
					{property: property},
					renderEscape
				);
				result.append(renderedProperty);

				var valueTemplate = $("#object-value-template").html();
				$(".values", result).append(
					values.map (function (value, index) {
						value = value || {};
						var res = $( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						
						// Search modal
						$(".search-component", res).on("click", function ( e ) {
							var $modal = $("#search-modal");
							var search = new SearchModel(veda, undefined, $(".modal-body", $modal) );
							
							$modal.modal();
							
							// Add found values
							$("button#ok", $modal).on("click", function (e) {
								var selected = [];
								for (var uri in search.selected) {
									selected.push( search.selected[uri] );
								}
								document[property.id] = values.concat(selected);
							});
							
						});
						
						// Toggle linked object view
						var $toggle = $(".toggle-expand", res),
							$linkContainer = $("<div />"),
							linkedDoc;
						$toggle.on("click", function ( e ) {
							$("i", $toggle).toggleClass("glyphicon-collapse-down glyphicon-collapse-up");
							$linkContainer.toggle();
							$toggle.after( $linkContainer );
							if (!linkedDoc) linkedDoc = new DocumentModel(veda, value, $linkContainer);
						});
						
						return res;
					})
				);
				
				$("[bound]", result).on("change", function ( e ) {
					var tmp = $("[bound]", result).map(function () {
						if (!this.value) return "";
						try { return new IndividualModel( veda, this.value ); } 
						catch (e) { return "" }
					}).get();
					document[property.id] = tmp;
				});
				break
		}

		$(".remove", result).on("click", function () {
			var $target = $(this.parentNode);
			$target.remove();
			var bound = $("[bound]", result);
			if (bound.length) return bound.first().trigger("change");
			else document[property.id] = [];
		});
		
		$(".add", result).on("click", function () {
			values.push(undefined);
			document[property.id] = values; 
		});

		return result;
	}
	
	veda.on("document:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		container.empty();
		
		document["rdf:type"]
			.filter( function (item) {
				return item instanceof IndividualModel
			})
			.map( function (item) {
				
				var _class = new ClassModel(veda, item);
				
				var template;
				
				// Get template from class
				if (_class.documentTemplate["v-ui:template"]) {
					template = _class.documentTemplate["v-ui:template"][0];
				} else {
					// Generic template
					template = $("<div/>").append($("#generic-class-template").html());
					$(".properties", template).append(
						Object.getOwnPropertyNames(_class.domainProperties()).map( function (property_uri) {
							if (property_uri == "rdfs:label") return;
							return $("<div/>", {"property":"document." + property_uri});
						})
					)
				}
				
				var renderedProperties = {};
				
				Object.getOwnPropertyNames(document.properties).reduce (function (acc, property_uri) {

					var property = document.properties[property_uri];
					var spec = _class.specsByProps[property_uri];
					var values = document[property_uri];
					acc[property_uri] = renderProperty(document, property, spec, values);
					return acc;
					
				}, renderedProperties);
				
				renderedProperties.id = document.id;
				
				var renderedDocument = $("<div/>");
				renderedDocument.append (template);
				
				var data = {
					document: renderedProperties, 
					_class: _class
				};
				
				$('[property]', renderedDocument).each( function () {
					var $this = $(this);
					var key = $this.attr("property").split(".").reduce(function (acc, i) {
						return isNaN(i) ? acc + "['" + i + "']" : acc + "[" + i + "]";
					}, "");
					var tmp = eval("data"+key);
					$this.empty().append( tmp instanceof Array ? tmp.join(", ") : tmp );
					$this.attr("data-id", document.id);
				});
				
				document.on("value:changed", function (property_uri, vals) {
					var property = document.properties[property_uri];
					var spec = _class.specsByProps[property_uri];
					var result = renderProperty (document, property, spec, vals);
					$(".view", result).hide();
					$("[property='document." + property_uri + "'][data-id='" + document.id + "']", renderedDocument).empty().append(result);
				});
				
				container.append( renderedDocument );
				
				$("textarea", renderedDocument).trigger("autosize.resize");
			
			});
		
		var $actions = $( $("#actions").html() );

		$(".edit", container).hide();
		$("#save, #cancel", $actions).hide();
				
		$("#edit", $actions).on("click", function (e) {
			$(".view", container).hide();
			$(".edit", container).show();
			
			$(this).hide();
			$("#save, #cancel", $actions).show();
		});

		$("#save", $actions).on("click", function (e) {
			document.save();
			$(".view", container).show();
			$(".edit", container).hide();
			
			$(this).hide();
			$("#cancel", $actions).hide();
			$("#edit", $actions).show();
		});

		$("#cancel", $actions).on("click", function (e) {
			document.reset();
		});
		
		container.append($actions);
		
		localize(container, veda.user.language);

	});

});

/*
Veda(function DocumentPresenter(veda) { "use strict";
	
	veda.on("document:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		container.empty();
		
		document["rdf:type"]
			.filter( function (item) {
				return item instanceof IndividualModel
			})
			.map( function (item) {
				
				var _class = new ClassModel(veda, item);
				
				// Get template from class
				var classTemplate = _class.documentTemplate["v-ui:template"][0];
				
				var $classTemplate = $(classTemplate.toString());
				
				$("[property]", $classTemplate).not("[about]").map( function () {
					
					var propertyContainer = $(this), 
						property_uri = propertyContainer.attr("property"),
						propertyTemplate = propertyContainer.attr("template"),
						values = document[property_uri];
						
					if (document[property_uri] instanceof Array) {
						document[property_uri].map( function (item) {
							var clone = propertyContainer.clone();
							clone.val(item);
							propertyContainer.after(clone);
						});
						propertyContainer.remove();
					} else {
						propertyContainer.val(document[property_uri]);
					}

				});
				
				$("[about]", $classTemplate).map( function () {
					
					var propertyContainer = $(this), 
						about = new IndividualModel(veda, propertyContainer.attr("about")),
						property_uri = propertyContainer.attr("property");

					propertyContainer.html( about[property_uri] );

				});

				$("[rel]", $classTemplate).map( function () {
					
					var relContainer = $(this), 
						rel_uri = relContainer.attr("rel"),
						relTemplate = relContainer.attr("template"),
						values = document[rel_uri];
					
					values.map( function (value) {
						relContainer.html(value.id );
					});
					
				});
				
				container.append($classTemplate);
				
			});
		
		var $actions = $( $("#actions").html() );

		$("#save, #cancel", $actions).hide();
				
		$("#edit", $actions).on("click", function (e) {
			$("input[disabled]", container).removeAttr("disabled");
			
			$(this).hide();
			$("#save, #cancel", $actions).show();
		});

		$("#save", $actions).on("click", function (e) {
			document.save();
			$("input", container).attr("disabled", "disabled");
			
			$(this).hide();
			$("#cancel", $actions).hide();
			$("#edit", $actions).show();
		});

		$("#cancel", $actions).on("click", function (e) {
			document.reset();
		});
		
		container.append($actions);
		
	});

});
*/

