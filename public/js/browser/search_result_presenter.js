// Search Result Presenter

Veda(function SearchResultPresenter(veda) { "use strict";
	
	// Get templates
	var displayedPropertiesLimit = 5;
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
		Object.getOwnPropertyNames(search_result.properties).reduce ( function (limit, property_uri) {
			if (limit <= 0) return limit;
			if (property_uri == "@") return limit;
			try {
				renderProperty (veda, search_result, property_uri, search_result_single_property_template, $("#search-result-properties", container));
			} catch (e) {}
			return --limit;
		}, displayedPropertiesLimit);

	});

});
