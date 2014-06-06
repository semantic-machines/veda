// User Presenter

Veda(function SearchPresenter(veda) { "use strict";

	var template = $("#search-template").html();
	var container = $("#main");
	var currentPage = 0;
		
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
		if (!search.results_count) {
			$("#search-results", container).addClass("hidden");
			$("#not-found", container).removeClass("hidden");
			return;
		}
		$("#not-found", container).addClass("hidden");
		$("#search-results", container).removeClass("hidden");
		$("#search-results-list", container)
			.empty()
			.attr("start", currentPage * veda.user.displayedElements + 1);
		$("#pager", container).empty();
		for (var page = 0; page < Math.floor(search.results_count / veda.user.displayedElements) + 1 * (search.results_count % veda.user.displayedElements ? 1 : 0); page++) {
			(function (page) {
				var $page = $("<li/>", {
					"class" : page == currentPage ? "active" : ""
				}).appendTo("#pager", container);
				var $a = $("<a/>", { 
					"text" : page + 1, 
					"click": function (event) {
						event.preventDefault(); 
						currentPage = page; 
						veda.trigger('search:complete', search);
					}, 
					"href" : ""
				}).appendTo($page);
			})(page);
		}
		for (var i = currentPage * veda.user.displayedElements; i < (currentPage + 1) * veda.user.displayedElements && i < search.results_count; i++) {
			var $li = $("<li/>").appendTo("#search-results-list");
			new DocumentModel(veda, [search.results[i], $li]);
		}
	});

});
