// Document Model

;(function (veda) { "use strict";

	/*veda.DocumentModel = function (uri, container, template, mode) {

		var self = new veda.IndividualModel(uri, true);

		self.off("*");
		
		self.on("individual:afterLoad individual:afterReset", function (event) {
			veda.trigger("document:afterLoad", self, container, template);
		});

		self.on("individual:typeChanged", function (event) {
			veda.trigger("document:afterLoad", self, container, template, mode);
		});

		veda.trigger("document:afterLoad", self, container, template, mode);

		return self;
	};*/

	veda.DocumentModel = function (uri, container, template, mode) {

		var individual = Object.create( new veda.IndividualModel(uri) );
		
		var self = riot.observable(individual);

		individual.on("individual:afterLoad individual:afterReset", function (event) {
			veda.trigger("document:afterLoad", self, container, template);
		});

		individual.on("individual:typeChanged", function (event) {
			veda.trigger("document:afterLoad", self, container, template, mode);
		});
				
		veda.trigger("document:afterLoad", self, container, template, mode);

		return self;
	};

})(veda);
