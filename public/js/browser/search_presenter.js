// User Presenter

Veda(function SearchPresenter(veda) { "use strict";

	veda.on("search:loaded", function (search) {
		
		// Get template
		var template = $("#search-template").html();
		
		var container = $("#main");
		container.html(template);

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

		// Display search results
		search.on("search:complete", function () {

		});

	});

});
