// Saved Search Presenter

veda.Module(function SavedSearchPresenter(veda) { "use strict";
	
	var saveSearchBtnTmpl = $("#save-search-template").html();
	
	veda.on("search:rendered", function (search, container_param) {
		
		var saveSearchBtn = $( saveSearchBtnTmpl );
		var container = container_param || $("#main");
		var qActions = $("#q-actions", container);
		
		$("#save-search", qActions).remove();
		qActions.prepend(saveSearchBtn);
		
		saveSearchBtn.on("click", function () {
			if (search.q) {
				
				var that = $(this);
				var ssContainer = $("<div/>");
				var ss = new veda.DocumentModel(undefined, ssContainer, undefined, "edit");
				ss["rdf:type"] = [new veda.IndividualModel("v-s:SavedSearch")];
				ss["v-s:author"] = [veda.user];
				ss["v-s:created"] = [new Date()];
				ss["v-s:query"] = [search.q];
				ss["rdfs:label"] = [search.q];

				ss.on("document:afterSave document:afterReset", function () {
					that.popover("destroy");
				});
				
				that.popover({
					html: true,
					content: ssContainer,
					placement: "auto",
					container: "body"
				}).popover("show").on('hidden.bs.popover', function () {
					that.popover("destroy");
				});
				
			}
		});
	});
});
