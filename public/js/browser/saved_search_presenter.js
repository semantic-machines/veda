// Document Presenter

Veda(function SavedSearchPresenter(veda) { "use strict";

	veda.on("search:rendered", function (search) {
		
		$("#save-search").off("click");
		
		$("#save-search").on("click", function () {
			var ss = new IndividualModel(veda);

			ss.addProperty("rdf:type");
			ss["rdf:type"] = [new IndividualModel(veda, "veda-schema:SavedSearch")];

			ss.addProperty("veda-schema:author"); 
			ss["veda-schema:author"] = [veda.user];
			
			ss.addProperty("veda-schema:created");
			ss["veda-schema:created"] = [new Date()];
			
			ss.addProperty("veda-schema:query");
			ss["veda-schema:query"] = [search.q];
			
			ss.save();
			
		});

	});
	
});
