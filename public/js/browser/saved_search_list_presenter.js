// Save Search Presenter

veda.Module(function SaveSearchPresenter(veda) { "use strict";
	
	var template = $("#saved-search-list-button-template").html();
	
	veda.on("search:rendered", function (search, container_param) {

		var q = "'rdf:type'=='v-s:SavedSearch'&&'v-s:author'=='" + veda.user.id + "'";
		
		var btn = $( template );
		var container = container_param || $("#main");
		var qActions = $("#q-actions", container);
		
		$("#saved-search-list", qActions).remove();
		qActions.prepend(btn);

		var sContainer = $("<div/>");
		btn.popover({
			html: true,
			content: sContainer,
			placement: "bottom",
			container: "body"
		});
		
		var ol = $("<ol>");
		var tmpl = new veda.IndividualModel("v-ui:LabelTemplate");
		btn.one("click", function () {
			var tmp = $("<div>");
			var s = new veda.SearchModel(q, tmp);
			Object.getOwnPropertyNames(s.results).map( function (id) {
				var li = $("<li>").appendTo(ol);
				var d = new veda.DocumentModel(s.results[id], li, tmpl);
				li.click(function () {
					search.q = d["v-s:query"][0];
					search.search();
				});
			});
			sContainer.append(ol);
		});

	});
});
