// User Model

"use strict";

function UserModel(veda, params) {
	var uri = params[0];
	var defaults = {
		language : "RU",
		displayedElements : 10
	};
	var self = riot.observable(this);
	
	self.on("individual:loaded", function() {
		try { 
			self.preferences = new IndividualModel(veda, [self["veda-ui:hasPreferences"][0]["@"]]);
			self.language = self.preferences["veda-ui:preferredLanguage"][0]["rdf:value"][0];
			self.displayedElements = self.preferences["veda-ui:displayedElements"][0];
		} catch (e) {
			self.language = defaults.language;
			self.displayedElements = defaults.displayedElements;
		}
		self.trigger("user:loaded");
	});
	
	// Model loaded message
	self.on("user:loaded", function(){
		if (veda) veda.trigger("user:loaded", self);
	});
		
	// Inherit from IndividualModel
	IndividualModel.call(this, veda, params);
};
