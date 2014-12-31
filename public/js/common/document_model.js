// Document Model

;(function (veda) { "use strict";

	veda.DocumentModel = function (uri, container, template, mode) {

		var individual = new veda.IndividualModel(uri);
		
		var self = riot.observable( Object.create(individual) );
		
		individual.on("individual:afterReset", function (event) {
			veda.trigger("document:loaded", self, container, template);
		});

		individual.on("individual:typeChanged", function (event) {
			veda.trigger("document:loaded", self, container, template, mode);
		});
		
		veda.trigger("document:loaded", self, container, template, mode);

		return self;
	};

})(veda);
