// Document Presenter

Veda(function SavedSearchPresenter(veda) { "use strict";

	veda.on("search:rendered", function (search) {
		
		$("#save-search").off("click");
		
		$("#save-search").on("click", function () {
			var ss = new IndividualModel(veda);

			ss.defineProperty("rdf:type");
			ss["rdf:type"] = [new IndividualModel(veda, "v-s:SavedSearch")];

			ss.defineProperty("v-s:author"); 
			ss["v-s:author"] = [veda.user];
			
			ss.defineProperty("v-s:created");
			ss["v-s:created"] = [new Date()];
			
			ss.defineProperty("v-s:query");
			ss["v-s:query"] = [search.q];
			
			ss.save();
			
		});

	});
	
});
