// Document Presenter

Veda(function SavedSearchPresenter(veda) { "use strict";

	veda.on("search:rendered", function (search) {
		
		$("#save-search").off("click");
		
		$("#save-search").on("click", function () {
			var ss = new IndividualModel(veda);

			ss.defineProperty("rdf:type");
			ss["rdf:type"] = [new IndividualModel(veda, "veda-schema:SavedSearch")];

			ss.defineProperty("veda-schema:author"); 
			ss["veda-schema:author"] = [veda.user];
			
			ss.defineProperty("veda-schema:created");
			ss["veda-schema:created"] = [new Date()];
			
			ss.defineProperty("veda-schema:query");
			ss["veda-schema:query"] = [search.q];
			
			ss.save();
			
		});

	});
	
});
