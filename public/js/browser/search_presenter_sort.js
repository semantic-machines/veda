veda.Module(function SearchPresenterSort(veda) { "use strict";

	var	sortPropertyContainer = $( $("#sort-property-template").html() ),
		sortPropertyTemplate = $(".sort-property", sortPropertyContainer)[0].outerHTML,
		query = "",
		sort = "",
		attributes = {},
		currentPage = 0,
		visitedPages = [];
	
	$(".sort-property", sortPropertyContainer).remove();
	
	veda.on("search:complete", function (search, container_param, page) {

		if (query != search.q || sort != search.sort) {
			query = search.q;
			sort = search.sort;
			visitedPages = [];
			attributes = {};
		}
		
		var $search_features = $("#search-features");
		var container = container_param || $("#main");
		if (!$("#sort-property-container", $search_features).length) $search_features.append(sortPropertyContainer);
		currentPage = page || 0;
		if (visitedPages.indexOf(currentPage) >= 0) return;
		visitedPages.push(currentPage);
		
		for (var i = currentPage * veda.user.displayedElements; i < (currentPage + 1) * veda.user.displayedElements && i < search.results_count ; i++) {
			var key = Object.getOwnPropertyNames(search.results)[i];
			var result = search.results[key];
			var properties = result.properties;
			Object.getOwnPropertyNames(properties).map( function (property_uri) {
				attributes[property_uri] = properties[property_uri];
			});
		}
		
		$(".sort-property", sortPropertyContainer).remove();
		var limit = 5;
		for (i in attributes) {
			if (limit-- === 0) break;
			var $sortProperty = $(sortPropertyTemplate);
			$(".property-name", $sortProperty).html(attributes[i]["rdfs:label"].join(", "));
			$("input", $sortProperty).change(function () {
				
			});
			sortPropertyContainer.append($sortProperty);
		}
		
		//console.log("query", query, "; sort", sort, "; visitedPages", visitedPages, "; attributes", attributes);

	});
});
