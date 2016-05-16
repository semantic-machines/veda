veda.Module(function SearchSort(veda) { "use strict";

	var	sortPropertyContainer = $( $("#sort-property-template").html() ),
		sortPropertyTemplate = $(".sort-property", sortPropertyContainer)[0].outerHTML,
		query = "",
		sort = "",
		properties = {},
		currentPage = 0,
		visitedPages = [],
		orderBy = {};
		
	function encodeSort() {
		var acc = [];
		for (var i in orderBy) {
			acc.push("'" + i + "' " + orderBy[i]);
		}
		return acc.join(",");
	}

	function decodeSort() {
		if (sort === "") return {};
		return sort.split(",").reduce(function (acc, item) {
			var split = item.split(" "),
				prop = split[0].replace(/'/g, ""),
				dir = split[1];
			acc[prop] = dir;
			return acc;
		}, {});
	}
	
	$(".sort-property", sortPropertyContainer).remove();
	
	veda.on("search:complete", function (search, container_param, page) {

		if (query != search.q || sort != search.sort) {
			query = search.q;
			sort = search.sort;
			visitedPages = [];
			properties = {};
			currentPage = 0;
			orderBy = decodeSort();
		}
		
		var $search_features = $("#search-features");
		var container = container_param || $("#main");
		if (!$("#sort-property-container", $search_features).length) $search_features.append(sortPropertyContainer);
		currentPage = page || 0;
		
		//if (visitedPages.indexOf(currentPage) >= 0) return;
		
		visitedPages.push(currentPage);
		
		for (var i = currentPage * veda.user.displayedElements; i < (currentPage + 1) * veda.user.displayedElements && i < search.results_count ; i++) {
			var key = Object.getOwnPropertyNames(search.results)[i];
			var result = search.results[key];
			Object.getOwnPropertyNames(result.properties).map( function (property_uri) {
				properties[property_uri] = new veda.IndividualModel(property_uri);
			});
		}
		
		$(".sort-property", sortPropertyContainer).remove();
		
		var limit = 5;
		Object.keys(properties).map(function (property_uri) {
			if (limit-- <= 0) return;
			var $sortProperty = $(sortPropertyTemplate);
			$(".property-name", $sortProperty).html(properties[property_uri]["rdfs:label"].join(", "));
			var checkbox = $("input", $sortProperty);
			var direction = $(".direction", $sortProperty);
			if ( property_uri in orderBy && orderBy[property_uri] == "asc") {
				checkbox.prop("checked", true);
				direction.addClass("glyphicon-sort-by-alphabet").removeClass("glyphicon-sort-by-alphabet-alt").removeClass("disabled");
			} else if ( property_uri in orderBy && orderBy[property_uri] == "desc") {
				checkbox.prop("checked", true);
				direction.addClass("glyphicon-sort-by-alphabet-alt").removeClass("glyphicon-sort-by-alphabet").removeClass("disabled");
			}
			checkbox.change(function () {
				if ( $(this).is(":checked") ) {
					direction.addClass("glyphicon-sort-by-alphabet").removeClass("glyphicon-sort-by-alphabet-alt").removeClass("disabled");
					orderBy[property_uri] = "asc";
					search.sort = sort = encodeSort();
					search.search();
				} else {
					direction.addClass("glyphicon-sort-by-alphabet").removeClass("glyphicon-sort-by-alphabet-alt").addClass("disabled");
					delete orderBy[property_uri];
					search.sort = sort = encodeSort();
					search.search();
				}
			});
			direction.click( function (e) {
				e.preventDefault();
				if ( orderBy[property_uri] == "asc" ) {
					orderBy[property_uri] = "desc";
					direction.addClass("glyphicon-sort-by-alphabet-alt").removeClass("glyphicon-sort-by-alphabet");
					search.sort = sort = encodeSort();
					search.search();
				} else {
					orderBy[property_uri] = "asc";
					direction.addClass("glyphicon-sort-by-alphabet").removeClass("glyphicon-sort-by-alphabet-alt");
					search.sort = sort = encodeSort();
					search.search();
				}
			});
			sortPropertyContainer.append($sortProperty);
		});

		//console.log("query", query, "; sort", sort, "; orderBy", orderBy, "; visitedPages", visitedPages, "; properties", properties);

	});
});
