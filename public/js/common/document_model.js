// Document Model

veda.Module(function DocumentModel(veda) { "use strict";

	veda.DocumentModel = function (uri, container, template, mode) {

		var individual = new veda.IndividualModel(uri);
		
		var self = riot.observable( Object.create(individual) );
		
		self.cancel = function () {
			self.reset();
			self.trigger("document:cleanup");
			self = new veda.DocumentModel(uri, container, template, "view");
		}

		function typeChangedHandler () {
			self = new veda.DocumentModel(individual, container, template, mode);
		}

		function propertyModifiedHandler (property_uri, values) {
			self.trigger("document:propertyModified", property_uri, values);
		}
		
		individual.on("individual:propertyModified", propertyModifiedHandler);
		individual.on("individual:typeChanged", typeChangedHandler);
		
		self.on("document:cleanup", function () {
			individual.off("individual:propertyModified", propertyModifiedHandler);
			individual.off("individual:typeChanged", typeChangedHandler);
			self.off("*");
			individual = self = null;
		});
		
		veda.trigger("document:loaded", self, container, template, mode);

		return self;
	};

});
