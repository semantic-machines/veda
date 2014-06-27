// Search Result Presenter

Veda(function SearchResultPresenter(veda) { "use strict";
	
	// Get templates
	var search_result_template = $("#search-result-template").html();
	var search_result_single_property_template = $("#search-result-single-property-template").html();
	var search_result_label_template = $("#search-result-label-template").html();
	
	veda.on("search_result:loaded", function (search_result, container_param) {
		
		var container = container_param || $("#main");
		container.html(search_result_template);

		// Render document title		
		$("#search-result-label", container).html( 
			riot.render(
				search_result_label_template,
				{ 
					label: search_result["rdfs:label"] ? search_result["rdfs:label"]
						.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
						.join(", ") : search_result["@"],
					uri: search_result["@"] 
				}
			) 
		);
		
		// Render document properties
		Object.getOwnPropertyNames(search_result.properties).map ( function (property_uri) {
			if (property_uri == "@") return;
			var label, uri, values;
			label = typeof search_result.properties[property_uri] == "object" ? 
						search_result.properties[property_uri]["rdfs:label"]
							.filter(function(item){return item.language == veda.user.language || item.language == "NONE"})
							.join(", ")
						: search_result.properties[property_uri];
			uri = typeof search_result.properties[property_uri] == "object" ? search_result.properties[property_uri]["@"] : "";
			values = search_result[property_uri]
						.map( function (item) {
							if (item instanceof String)
								// Check if string starts with http:// or ftp://
								return item.search(/^.{3,5}:\/\//) == 0 ? "<a target='_blank' href='" + item + "'>" + item + "</a>" : item ;
							else if (item instanceof IndividualModel)
								return "<a href='#/document/" + item["@"] + "'>" + 
									(item["rdfs:label"] ? item["rdfs:label"].filter(function(item){return item.language == veda.user.language || item.language == "NONE"}).join(", ") : item["@"]) + "</a>";
							else return item;
						})
						.filter(function(item){return item.language ? item.language == veda.user.language || item.language == "NONE" : item})
						.join(", ");
			$("#search-result-properties", container).append(
				riot.render(
					search_result_single_property_template,
					{
						label: label,
						uri: uri,
						values: values
					}
				)
			);
		});

	});

});
