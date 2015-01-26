veda.Module(function SearchPresenterSort(veda) { "use strict";

	var button = $("#sort-button-template").html();
	var popover = $("#sort-popover-template").html();
	
	veda.on("search:complete", function (search, container_param, page) {

		var container = container_param || $("#main");

		$("#search-features").append(button);
		
		var attributes = {}; 
		var tst = "";

		for (var i = page * veda.user.displayedElements; (page + 1) * veda.user.displayedElements ; i++) {
			var key = Object.getOwnPropertyNames(search.results)[i];
			var result = search.results[key];
			var properties = result.properties;
			properties.map( function (item) {
				attribites[item.id] = item;
				tst += item.id;
			});
		}
		
		console.log("attrs", attributes);
		
		$("[data-toggle='popover']", container).popover({
			title: "<span>Порядок сортировки</span>",
			content: tst,
			html: true,
			placement: "auto",
			container: "body"
		});

	});
});
