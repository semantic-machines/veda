// Document Presenter

Veda(function DocumentPresenter(veda) { "use strict";
	
	var cnt = 0;
	
	veda.on("document:loaded", function (document, container_param, template) {
		
		console.log("document presenter:", ++cnt, document.id);
		
		var container = container_param || $("#main");
		
		container.empty().hide();
		
		var embedded = [];
				
		document["rdf:type"]
			.filter( function (item) {
				return item instanceof IndividualModel
			})
			.map( function (item) {
				
				var _class = new ClassModel(veda, item);
				
				var classTemplate;
				
				if (template) { 
					classTemplate = $( template["v-ui:template"][0].toString() ); 
				} else if (_class.documentTemplate["v-ui:template"]) {
					// Get template from class
					classTemplate = $( _class.documentTemplate["v-ui:template"][0].toString() );
				} else {
					// Construct generic template
					classTemplate = $("<div/>").append( $("#generic-class-template").html() );
					$(".properties", classTemplate).append (
						Object.getOwnPropertyNames(_class.domainProperties).map( function (property_uri, index, array) {
							var property = _class.domainProperties[property_uri];
							if (property_uri == "rdfs:label") return;
							
							var result = $("<div/>").append( 
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
									result.append( $("<div/>", {"property": property_uri}) ); 
								break
								default:
									result.append( $("<div/>", {"rel": property_uri}) ); 
								break
							}
							
							if (index < array.length-1) result.append( $("<hr/>").attr("style", "margin: 10px 0px") ); 
							
							return result;
						})
					)
				}

				// Actions
				var $edit = $("#edit", classTemplate),
					$save = $("#save", classTemplate),
					$cancel = $("#cancel", classTemplate);
				
				$edit
					.on("click", function (e) {
						document.trigger("edit");
					});
				
				$save.hide()
					.on("click", function (e) {
						document.trigger("save");
					});
				
				$cancel.hide()
					.on("click", function (e) {
						document.trigger("cancel");
					});
				
				document.off("view edit save cancel");
				
				// Trigger same events for embedded templates
				document.on("edit save cancel", function (event) {
					embedded.map(function (item) {
						item.trigger(event);
					});
				});
				
				document.on("edit", function () {
					$edit.hide();
					$save.show();
					$cancel.show();
				});

				document.on("save", function () {
					document.save();
					$save.hide();
					$cancel.hide();
					$edit.show();
				});

				document.on("cancel", function () {
					document.reset();
				});

				// About
				$("[about]", classTemplate).map( function () {
					
					var propertyContainer = $( this ), 
						about = new IndividualModel(veda, propertyContainer.attr("about")),
						property_uri = propertyContainer.attr("property");
					if (property_uri == "id") propertyContainer.html( about[property_uri] );
					else propertyContainer.html( about[property_uri].join(", ") );

				});

				// Object links				
				$("[rel]", classTemplate).map( function () {
					
					var relContainer = $(this), 
						rel_uri = relContainer.attr("rel"),
						relTemplate = relContainer.attr("template"),
						values = document[rel_uri];
					
					relTemplate = relTemplate ? 
						new IndividualModel(veda, relTemplate) 
						:
						new IndividualModel(veda, "mnd-d:LabelTemplate");
					
					if (values) {
						values.map( function (value) {
							var clone = relContainer.clone();
							if (value instanceof IndividualModel) {
								setTimeout( function () {
									var lnk = new DocumentModel(veda, value, clone, relTemplate);
									if (relTemplate["v-ui:embedded"] && relTemplate["v-ui:embedded"][0]) embedded.push(lnk);
								}, 0);
							}
							relContainer.before(clone);
						});
					}
					relContainer.remove();
				});

				// Properties
				$("[property]", classTemplate).not("[about]").map( function () {
					
					var propertyContainer = $(this), 
						property_uri = propertyContainer.attr("property"),
						propertyTemplate = propertyContainer.attr("template");
						
					renderProperty(document, property_uri, propertyContainer);
					
					document.on(property_uri+":changed", function() {
						renderProperty(document, property_uri, propertyContainer);
						document.trigger("edit");
					});

				});
				
				// Specials
				$("[href='id']", classTemplate).map( function () {
					$( this ).attr("href", "#/document/" + document.id);
				});
				
				container.append(classTemplate);
				
				document.trigger("view");
				
				container.fadeIn(250);
				
			});
	});
	
	
	function renderProperty (document, property_uri, container) {
		
		if ( property_uri != 'id' && !veda.dictionary[property_uri] ) return;
		
		container.empty();
		
		if (property_uri == "id") { 
			container.val(document[property_uri]).html(document[property_uri]); 
			return;
		}
		
		var property = veda.dictionary[property_uri],
			template, renderedProperty;

		if ( !document[property_uri] ) document.defineProperty(property_uri);
		var values = document[property_uri];

		switch( property["rdfs:range"] ? property["rdfs:range"][0].id : "rdfs:Literal" ) {

			case "rdfs:Literal" : 
			case "xsd:string" : 
				template = $("#string-control-template").html();

				if (!values.length) values.push("");

				values.map (function (value, index) {
					
					var $template = $(template),
						$view = $(".view", $template),
						$edit = $(".edit", $template);
					
					document.on("edit", function() {
						$view.hide();
						$edit.show();
					});
					document.on("view", function() {
						$view.show();
						$edit.hide();
					});

					$("textarea", $template)
						.autosize()
						.on("focus", function (event) {
							$(this).trigger("autosize.resize");
						});
					
					$("[bound]", $template)
						.html(value)
						.val(value)
						.data("language", value.language)
						.on("change", function ( e ) {
							document[property.id] = $(".edit > [bound]", container).map(function () {
								var res = new String(this.value);
								res.language = $(this).data("language");
								return res;
							}).get();
						});
					
					$(".language-selector", $template).prepend(value.language);
					
					$(".remove", $template).on("click", function () {
						var $target = $(this.parentNode);
						$target.remove();
						var bound = $(".edit > [bound]", container);
						if (bound.length) return bound.first().trigger("change");
						else document[property_uri] = [];
					});

					$(".add", $template).on("click", function () {
						var emptyVal = new String(""); emptyVal.language = undefined;
						values.push(emptyVal);
						document[property_uri] = values;
					});

					var $first = $("<li>").append( $("<a>", {href: "#", "data-language": "", text: "-"}).addClass("language") );
					if (!value.language) $first.addClass("active");
					$(".language-list", $template).append(
						$first,
						Object.keys(veda.availableLanguages).map(function (language_name) {
							var $li = $("<li>"), 
								$a = $("<a>", {href: "#", "data-language": language_name, text: language_name}).addClass("language");
							$li.append($a);
							if (value.language == language_name) $li.addClass("active");
							return $li;
						})
					);
					
					$(".language", $template).on("click", function ( e ) {
						e.preventDefault();
						$(".language-selector", $template)
							.empty()
							.append($(this).data("language"), " <span class='caret'></span>");
						$("textarea", $template)
							.data("language", $(this).data("language") )
							.trigger("change");
					});
					
					container.append($template);
				});
				
				return; 
				break

			case "xsd:boolean" : 
				template = $("#boolean-control-template").html();

				if (!values.length) values.push(new Boolean(false));

				values.map (function (value, index) {
					var $template = $(template),
						$view = $(".view", $template),
						$edit = $(".edit", $template);
						
					document.on("edit", function() {
						$view.hide();
						$edit.show();
					});
					document.on("view", function() {
						$view.show();
						$edit.hide();
					});
					$("[bound]", $template)
						.html(value)
						.val(value)
						.on("change", function ( e ) {
							document[property_uri] = $(".edit > [bound]", container).map(function () {
								return new Boolean(this.value == "true" ? true : false);
							}).get();
						});
					container.append($template);
				});
				break
		
			case "xsd:nonNegativeInteger" : 
			case "xsd:integer" : 
				template = $("#integer-control-template").html();

				if (!values.length) values.push(undefined);

				values.map (function (value, index) {
					var $template = $(template),
						$view = $(".view", $template),
						$edit = $(".edit", $template);
						
					document.on("edit", function() {
						$view.hide();
						$edit.show();
					});
					document.on("view", function() {
						$view.show();
						$edit.hide();
					});
					$("[bound]", $template)
						.html(value)
						.val(value)
						.on("change", function ( e ) {
							document[property_uri] = $(".edit > [bound]", container).map(function () {
								var value = "", 
									int = parseInt(this.value, 10);
								if ( isNaN(int) == false ) value = new Number(int);
								return value;
							}).get();
						});
					container.append($template);
				});
				break
			
			case "xsd:decimal" : 
				template = $("#decimal-control-template").html();

				if (!values.length) values.push(undefined);
				
				values.map (function (value, index) {
					var $template = $(template),
						$view = $(".view", $template),
						$edit = $(".edit", $template);
						
					document.on("edit", function() {
						$view.hide();
						$edit.show();
					});
					document.on("view", function() {
						$view.show();
						$edit.hide();
					});
					$("[bound]", $template)
						.html(value)
						.val(value)
						.on("change", function ( e ) {
							document[property_uri] = $(".edit > [bound]", container).map(function () {
								var value = "", 
									float = parseFloat(this.value);
								if ( isNaN(float) == false ) value = new Number(float);
								return value;
							}).get();
						});
					container.append($template);
				});
				break

			case "xsd:dateTime" : 
				template = $("#datetime-control-template").html();

				if (!values.length) values.push(undefined);
				
				values.map (function (value, index) {
					var $template = $(template),
						$view = $(".view", $template),
						$edit = $(".edit", $template);
						
					document.on("edit", function() {
						$view.hide();
						$edit.show();
					});
					document.on("view", function() {
						$view.show();
						$edit.hide();
					});
					$("[bound]", $template)
						.html(value)
						.val(value)
						.on("change", function ( e ) {
							document[property_uri] = $(".edit > [bound]", container).map(function () {
								var value = "", 
									timestamp = Date.parse(this.value);
								if ( isNaN(timestamp) == false ) value = new Date(timestamp);
								return value;
							}).get();
						});
					container.append($template);
				});
				break
		}

		$(".remove", container).on("click", function () {
			var $target = $(this.parentNode);
			$target.remove();
			var bound = $(".edit > [bound]", container);
			if (bound.length) return bound.first().trigger("change");
			else document[property_uri] = [];
		});
		
		$(".add", container).on("click", function () {
			values.push(undefined);
			document[property_uri] = values;
		});

	}

});



