// Document Model

;(function (veda) { "use strict";

	veda.DocumentModel = function (individual, container, template, mode) {

		var self;
		
		if (individual instanceof veda.IndividualModel) self = individual;
		else self = new veda.IndividualModel(individual, true);

		self.off("*");
		
		self.on("individual:loaded individual:reset", function (event) {
			veda.trigger("document:loaded", self, container, template);
		});

		veda.trigger("document:loaded", self, container, template, mode);

		return self;
	};

}(veda));
