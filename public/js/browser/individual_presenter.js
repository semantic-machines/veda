// Search Result Presenter

Veda(function IndividualPresenter (veda) { "use strict";
	
	function renderIndividualProperty (veda, individual, property_uri, template, container) {
		var label, uri, values;
		label = typeof individual.properties[property_uri] == "object" ? 
					individual.properties[property_uri]["rdfs:label"].join(", ")
					: individual.properties[property_uri];
		uri = typeof individual.properties[property_uri] == "object" ? individual.properties[property_uri].id : "";
		values = individual[property_uri]
					.map( function (item) {
						if (item instanceof String)
							// Check if string starts with http:// or ftp://
							return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
						else if (item instanceof IndividualModel)
							return "<a data-toggle='popover' href='#/document/" + item.id + "'>" + 
								(item["rdfs:label"] ? item["rdfs:label"].join(", ") : item.id) + "</a>";
						else return item;
					})
					.join(", ");
		container.append (
			riot.render (
				template,
				{
					label: label,
					uri: uri,
					values: values
				}
			)
		);
	}
	
	// Get templates
	var displayedPropertiesLimit = 5;
	var individual_template = $("#individual-template").html();
	var individual_single_property_template = $("#individual-single-property-template").html();
	var individual_label_template = $("#individual-label-template").html();
	
	veda.on("individual:loaded", function (individual, container) {
		
		if (!container) return;
		
		container.append(individual_template);
		localize(container, veda.user.language);
		
		// Render document title		
		$("#individual-label", container).append( 
			riot.render(
				individual_label_template,
				{ 
					label: individual["rdfs:label"].length ? individual["rdfs:label"].join(", ") : individual.id,
					uri: individual.id 
				}
			)
		);
		
		// Render document properties
		Object.getOwnPropertyNames(individual.properties).reduce ( function (limit, property_uri) {
			if (limit <= 0) return limit;
			try {
				renderIndividualProperty (veda, individual, property_uri, individual_single_property_template, $("#individual-properties", container));
			} catch (e) {}
			return --limit;
		}, displayedPropertiesLimit);

	});

	// Individual popover
	(function () {
		$("body").on("mouseenter", "[data-toggle='popover']", function () {
			var popover_element = $( this );
			var uri = popover_element.attr("href");
			uri = uri.substring(uri.indexOf("#/document/") + "#/document/".length);
			var thisTimeout = setTimeout( function () {
				if ($("#popover_"+escape4$(uri)).length) {
					
					var popover = popover_element.popover({
						content: $("#popover_"+escape4$(uri)).html(),
						html: true,
						placement: "auto",
						container: "body"
					}).popover("show");
					
				} else {
					
					var container = $("<div/>", {
						id: "popover_" + uri,
						class: "hide",
					}).appendTo("body");
					
					new IndividualModel(veda, uri, container);
					
					var popover = popover_element.popover({
						content: container.html(),
						html: true,
						placement: "auto",
						container: "body"
					}).popover("show");
					
				}
			}, 700);
			popover_element.mouseleave ( function () {
				clearTimeout(thisTimeout);
				popover_element.popover("destroy");
			});
		});
	})();

});
