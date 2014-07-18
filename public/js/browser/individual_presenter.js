// Search Result Presenter

Veda(function IndividualPresenter(veda) { "use strict";
	
	function renderIndividualProperty (veda, individual, property_uri, template, container) {
		var label, uri, values;
		label = typeof individual.properties[property_uri] == "object" ? 
					individual.properties[property_uri]["rdfs:label"]
						.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
						.join(", ")
					: individual.properties[property_uri];
		uri = typeof individual.properties[property_uri] == "object" ? individual.properties[property_uri]["@"] : "";
		values = individual[property_uri]
					.map( function (item) {
						if (item instanceof String)
							// Check if string starts with http:// or ftp://
							return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
						else if (item instanceof IndividualModel)
							return "<a data-toggle='popover' href='#/document/" + item["@"] + "'>" + 
								(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
						else return item;
					})
					.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
					.join(", ");
		container.append(
			riot.render(
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
		
		container.html(individual_template);
		localize(container, veda.user.language);
		
		// Render document title		
		$("#individual-label", container).html( 
			riot.render(
				individual_label_template,
				{ 
					label: individual["rdfs:label"] ? individual["rdfs:label"]
						.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
						.join(", ") : individual["@"],
					uri: individual["@"] 
				}
			) 
		);
		
		// Render document properties
		Object.getOwnPropertyNames(individual.properties).reduce ( function (limit, property_uri) {
			if (limit <= 0) return limit;
			if (property_uri == "@") return limit;
			try {
				renderIndividualProperty (veda, individual, property_uri, individual_single_property_template, $("#individual-properties", container));
			} catch (e) {}
			return --limit;
		}, displayedPropertiesLimit);

	});

});
