// User Presenter

Veda(function SearchPresenter(veda) { "use strict";

	var template = $("#search-template").html();
	var container = $("#main");

	veda.on("search:loaded", function (search) {
		
		// Get template
		var rendered = riot.render(template, search);
		container.html(rendered);

		// Listen View changes & update Model
		$("#search [bound]", container).on("change", function() {
			search[this.id] = $(this).val();
		});
		
		$("#search #search-submit", container).on("click", function(event) {
			event.preventDefault();
			search.search();
		});
	
		// Listen Model changes & update View
		search.on("property:changed", function(property, value) {
			var $el = $("#search #" + property + "[bound]", container);
			if ($el.is("input, textarea, select")) $el.val( value );
			else $el.html( value );
		});
	});
	
	// Display search results
	veda.on("search:complete", function (search) {
		$("#search-results-list", container)
			.empty()
			.attr("start", search.currentPage * veda.user.displayedElements + 1);
		$("#pager", container).empty();
		for (var page = 0; page < Math.floor(search.results_count / veda.user.displayedElements) + 1; page++) {
			(function (page) {
				var $page = $("<li/>", {
					"class" : page == search.currentPage ? "active" : ""
				}).appendTo("#pager", container);
				var $a = $("<a/>", { 
					"text" : page + 1, 
					"click": function (event) {
						event.preventDefault(); 
						search.currentPage = page; 
						veda.trigger('search:complete', search);
					}, 
					"href" : ""
				}).appendTo($page);
			})(page);
		}
		for (var i = search.currentPage * veda.user.displayedElements; i < (search.currentPage + 1) * veda.user.displayedElements && i < search.results_count; i++) {
			var $li = $("<li/>").appendTo("#search-results-list");
			new DocumentModel(veda, [search.results[i], $li]);
		}
	});

});
