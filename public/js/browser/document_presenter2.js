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
						return riot.render(valueTemplate, {value: value, index: index, property: property});
					})
				);
				$("[bound]", result).on("change", function ( e ) {
					var tmp = $("[bound]", result).map(function () {
						if (!this.value) return this.value;
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
				if (_class.documentTemplate["veda-ui:template"]) {
					template = _class.documentTemplate["veda-ui:template"][0]
				} else {
					// Generic template
					template = $("<div/>").append($("#generic-class-template").html());
					$(".properties", template).append(
						Object.getOwnPropertyNames(document.properties).map( function (property_uri) {
							if (property_uri == "rdfs:label") return;
							return $("<div/>", {"data-property":"document." + property_uri});
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
				
				$('[data-property]', renderedDocument).each( function () {
					var $this = $(this);
					var key = $this.data("property").split(".").reduce(function (acc, i) {
						return isNaN(i) ? acc + "['" + i + "']" : acc + "[" + i + "]";
					}, "");
					var tmp = eval("data"+key);
					$this.empty().append( tmp instanceof Array ? tmp.join(", ") : tmp );
				});
				
				document.on("value:changed", function (property_uri, vals) {
					var property = document.properties[property_uri];
					var spec = _class.specsByProps[property_uri];
					var result = renderProperty (document, property, spec, vals);
					$(".view", result).hide();
					$(".edit").show();
					$('[data-property="document.' + property_uri + '"]', renderedDocument).empty().append(result);
				});
				
				container.append( renderedDocument );
				
			});
		
		var actionsTemplate = $("#actions").html();
		container.append(actionsTemplate);

		$(".edit").hide();
		$("#save, #cancel", container).hide();
				
		$("#edit", container).on("click", function (e) {
			$(".view").hide();
			$(".edit").show();
			
			$(this).hide();
			$("#save, #cancel", container).show();
		});

		$("#save", container).on("click", function (e) {
			document.save();
			$(".view").show();
			$(".edit").hide();
			
			$(this).hide();
			$("#cancel", container).hide();
			$("#edit", container).show();
		});

		$("#cancel", container).on("click", function (e) {
			
			new DocumentModel(veda, document.id, container_param);
			
			/*$(".view").show();
			$(".edit").hide();
			
			$(this).hide();
			$("#edit", container).show();
			$("#save", container).hide();*/
		});

		localize(container, veda.user.language);

	});

});
