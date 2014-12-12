// Document Model

;(function (veda) { "use strict";

	veda.DocumentModel = function (individual, container, template, mode) {

		var self;
		
		if (individual instanceof veda.IndividualModel) self = individual;
		else self = new veda.IndividualModel(individual, true);

		self.off("*");
		
		self.on("individual:afterLoad individual:afterReset", function (event) {
			veda.trigger("document:afterLoad", self, container, template);
		});

		self.on("individual:typeChanged", function (event) {
			veda.trigger("document:afterLoad", self, container, template, mode);
		});

		veda.trigger("document:afterLoad", self, container, template, mode);

		return self;
	};

}(veda));
