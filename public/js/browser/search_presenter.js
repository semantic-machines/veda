// Search Presenter

veda.Present(function Search(veda) { "use strict";

	var template = $("#search-template").html();
	var currentPage = 0;
	
	// Initialize search page
	veda.on("search:loaded", function (search, container_param) {
		
		var container = container_param || $("#main");
		
		container.hide();
		
		// Get template
		var rendered = riot.render(template, search);
		container.html(rendered);
		//localize(container, veda.user.language);
		
		$("#q", container).focus();
		$("#not-found", container).hide();
		$("#search-results", container).hide();

		// Listen View changes & update Model
		$("#search [bound]", container).on("change", function() {
			search[this.id] = $(this).val();
		});
		
		$("#search #search-submit", container).on("click", function(event) {
			event.preventDefault();
			$("#search-submit").addClass("disabled"); 
			currentPage = 0;
			
			if (!container_param) return riot.route("#/search/" + search.q, true);
			search.search();
		});
	
		// Listen Model changes & update View
		search.on("property:changed", function(property, value) {
			var $el = $("#search #" + property + "[bound]", container);
			if ($el.is("input, textarea, select")) $el.val( value );
			else $el.html( value );
		});
		
		$("#select-all", container).on("click", function (e) {
			search.toggleAll();
			// Redraw current page
			veda.trigger("search:complete", search, container);
		});
		
		container.show();	
	});
	
	// Display search results
	veda.on("search:complete", function (search, container_param) {
		
		if (search.results_count < currentPage * veda.user.displayedElements) 
			currentPage = Math.floor(search.results_count / veda.user.displayedElements) + 1 * (search.results_count % veda.user.displayedElements ? 1 : 0) - 1;
		
		var container = container_param || $("#main");
		
		// Show/hide 'results' or 'not found'
		$("#search-submit").removeClass("disabled");
		if (!search.q) {
			$("#q", container).focus();
			$("#search-results", container).hide();
			$("#not-found", container).hide()
			return;
		} else if (search.q && !search.results_count) {
			$("#q", container).focus();
			$("#search-results", container).hide();
			$("#not-found", container).show();
			return;
		}
		$("#not-found", container).hide();
		$("#search-results", container).show();
		$("#search-results-list", container)
			.empty()
			.attr("start", currentPage * veda.user.displayedElements + 1);
		$("#pager", container).empty();
		
		// Show results
		for (var i = currentPage * veda.user.displayedElements; i < (currentPage + 1) * veda.user.displayedElements && i < search.results_count; i++) {
			(function (i) { 
				setTimeout(function () {

					var $li = $("<li/>").appendTo("#search-results-list");

					// Select search results 
					var $select = $( $("#search-select-template").html() );
					$("input[type='checkbox']", $select).on("click", function (e) {
						search.toggleSelected(i);
					});
					$li.append( $select );

					var search_result = new veda.SearchResultModel(search.results[i], $li);
					if (search_result.id in search.selected) $("input", $select).attr("checked", "checked");

				}, 0);
			}(i));
		}

		// Show pager
		var $pager = $("#pager", container);
		for (var page = 0; page < Math.floor(search.results_count / veda.user.displayedElements) + 1 * (search.results_count % veda.user.displayedElements ? 1 : 0); page++) {
			var $page = $("<li/>")
				.attr("class", page == currentPage ? "active" : "")
				.appendTo($pager);
			var $a = $("<a/>", { 
				"text" : page + 1, 
				"click": (function (page) {
					return function (event) {
						event.preventDefault(); 
						currentPage = page; 
						veda.trigger('search:complete', search, container);
					}
				})(page), 
				"href" : ""
			}).appendTo($page);
		}
	
	});

});
