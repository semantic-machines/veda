// Document Presenter

Veda(function DocumentPresenter2(veda) { "use strict";
	
	function renderProperty (document, property, spec, values) {
		
		var propertyTemplate = $("#property-template").html();
		
		var renderedProperty = "";
		
		renderedProperty = riot.render (
			propertyTemplate, 
			{property: property}
		);
		
		var result = $("<div/>");
		result.append(renderedProperty);
		
		var renderedValues = (function renderValue() {
		
			var res = $("<div/>");
			
			switch( property["rdfs:range"][0].id ) {

				case "rdfs:Literal" : 
				case "xsd:string" : 
					var valueTemplate = $("#string-value-template").html();
					values
						.map (function (value, index) {
							res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						});
					$("textarea", res).autosize(); 
					$("[bound]", res).on("change", function ( e ) {
						document[property.id] = $("[bound]", res).map(function () {
							return new String(this.value);
						}).get();
					});
					break

				case "xsd:boolean" : 
					var valueTemplate = $("#boolean-value-template").html();
					values
						.map (function (value, index) {
							res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						});
					$("[bound]", res).on("change", function ( e ) {
						document[property.id] = $("[bound]", res).map(function () {
							return new Boolean(this.value == "true" ? true : false);
						}).get();
					});
					break

				case "xsd:integer" : 
					var valueTemplate = $("#integer-value-template").html();
					values
						.map (function (value, index) {
							res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						});
					$("[bound]", res).on("change", function ( e ) {
						document[property.id] = $("[bound]", res).map(function () {
							return new Number( parseInt(this.value, 10) );
						}).get();
					});
					break
				
				case "xsd:decimal" : 
					var valueTemplate = $("#decimal-value-template").html();
					values
						.map (function (value, index) {
							res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						});
					$("[bound]", res).on("change", function ( e ) {
						document[property.id] = $("[bound]", res).map(function () {
							return new Number( parseFloat(this.value) );
						}).get();
					});
					break

				case "xsd:dateTime" : 
					var valueTemplate = $("#datetime-value-template").html();
					values
						.map (function (value, index) {
							res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						});
					$("[bound]", res).on("change", function ( e ) {
						document[property.id] = $("[bound]", res).map(function () {
							return new Date( this.value );
						}).get();
					});
					break

				default : 
					var valueTemplate = $("#object-value-template").html();
					values
						.map (function (value, index) {
							value = value || {};
							res.append( riot.render(valueTemplate, {value: value, index: index, property: property}) );
						});
					$("[bound]", res).on("change", function ( e ) {
						document[property.id] = $("[bound]", res).map(function () {
							if (!this.value) return undefined;
							return new IndividualModel( veda, this.value );
						}).get();
					});
					break
			}

			$(".remove", res).on("click", function () {
				var $target = $(this.parentNode);
				$target.remove();
				var bound = $("[bound]", res);
				if (bound.length) return bound.first().trigger("change");
				else document[property.id] = [];
			});
			
			return res;
		
		})();

		$(".values", result).append( renderedValues );
		
		$(".add", result).on("click", function () {
			values.push(undefined);
			document[property.id] = values; 
		});

		return result;
	}
	
	veda.on("document2:loaded", function (document, container_param) {
		
		var container = container_param || $("#main");
		container.empty();
		
		document["rdf:type"]
			.filter( function (item) {
				return item instanceof IndividualModel
			})
			.map( function (item) {
				
				var _class = new ClassModel(veda, item);
				
				var template;
				
				if (_class.documentTemplate["veda-ui:template"]) {
					template = _class.documentTemplate["veda-ui:template"][0]
				} else {
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
					$this.empty().append( eval("data"+key) );
				});
				
				document.on("value:changed", function (property_uri, vals) {
					var property = document.properties[property_uri];
					var spec = _class.specsByProps[property_uri];
					var result = renderProperty (document, property, spec, vals);
					$(".value", result).hide();
					$(".input-control").show();
					$('[data-property="document.' + property_uri + '"]', renderedDocument).empty().append(result);
				});
				
				container.append( renderedDocument );
				
			});
		
		var actionsTemplate = $("#actions").html();
		container.append(actionsTemplate);

		$(".input-control").hide();
		$("#save, #cancel", container).hide();
				
		$("#edit", container).on("click", function (e) {
			$(".value").hide();
			$(".input-control").show();
			
			$(this).hide();
			$("#save, #cancel", container).show();
		});

		$("#save", container).on("click", function (e) {
			document.save();
			$(".value").show();
			$(".input-control").hide();
			
			$(this).hide();
			$("#cancel", container).hide();
			$("#edit", container).show();
		});

		$("#cancel", container).on("click", function (e) {
			new DocumentModel2(veda, document.id, container_param);
			/*$(".value").show();
			$(".input-control").hide();
			
			$(this).hide();
			$("#edit", container).show();
			$("#save", container).hide();*/
		});

		localize(container, veda.user.language);

	});

});
