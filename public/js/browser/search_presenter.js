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
		$("#search-results-list", container).empty();
		for (var i = 0; i < search.results_count && i < veda.user.displayedElements; i++) {
			var $li = jQuery("<li/>").appendTo("#search-results-list");
			new DocumentModel(veda, [search.results[i], $li]);
		}
	});

});
