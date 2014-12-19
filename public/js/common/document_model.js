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

		var individual = new veda.IndividualModel(uri);
		
		var self = riot.observable( Object.create(individual) );
		
		individual.on("individual:propertyModified", function (property_uri, values) {
			//console.log(property_uri, values);
			//self.trigger("document:propertyModified", property_uri, values);
		});

		individual.on("individual:typeChanged", function (event) {
			veda.trigger("document:loaded", self, container, template, mode);
		});
				
		veda.trigger("document:loaded", self, container, template, mode);

		return self;
	};

})(veda);
