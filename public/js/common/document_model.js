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
		
		self.delete = function () {
			self["v-s:deleted"] = [new Boolean(true)];
			self.save();
		}

		self.recover = function () {
			self["v-s:deleted"] = [];
			self.save();
		}
		
		function typeChangedHandler () {
			self = new veda.DocumentModel(individual, container, template, mode);
		}
		function propertyModifiedHandler (property_uri, values) {
			self.trigger("document:propertyModified", property_uri, values);
		}
		function beforeSaveHandler () {
			self.trigger("document:beforeSave");
		}
		function afterSaveHandler () {
			self.trigger("document:afterSave");
		}
		function beforeResetHandler () {
			self.trigger("document:beforeSave");
		}
		function afterResetHandler () {
			self.trigger("document:afterSave");
		}

		individual.on("individual:propertyModified", propertyModifiedHandler);
		individual.on("individual:typeChanged", typeChangedHandler);
		individual.on("individual:beforeSave", beforeSaveHandler);
		individual.on("individual:afterSave", afterSaveHandler);
		individual.on("individual:beforeReset", beforeResetHandler);
		individual.on("individual:afterReset", afterResetHandler);
		
		self.on("document:cleanup", function () {
			individual.off("individual:propertyModified", propertyModifiedHandler);
			individual.off("individual:typeChanged", typeChangedHandler);
			individual.off("individual:beforeSave", beforeSaveHandler);
			individual.off("individual:afterSave", afterSaveHandler);
			individual.off("individual:beforeReset", beforeResetHandler);
			individual.off("individual:afterReset", afterResetHandler);
			self.off("*");
			individual = self = null;
		});
		
		veda.trigger("document:loaded", self, container, template, mode);

		return self;
	};

});
