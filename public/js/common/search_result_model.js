// Search result Model

;(function (veda) { "use strict";

	veda.SearchResultModel = function (individual, container, template) {

		var self = new veda.IndividualModel(individual);

		self.off("*");
		
		self.on("individual:loaded individual:reset individual:saved", function (event) {
			veda.trigger("search_result:loaded", self, container, template);
		});
		
		veda.trigger("search_result:loaded", self, container, template);

		return self;
	};

})(veda);
