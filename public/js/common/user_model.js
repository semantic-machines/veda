// User Model

"use strict";

function UserModel(veda, individual) {
	var defaults = {
		language : "RU",
		displayedElements : 10
	};
	
	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);
	
	try { 
		self.preferences = new IndividualModel(veda, self["veda-ui:hasPreferences"][0].id);
		self.language = self.preferences["veda-ui:preferredLanguage"][0]["rdf:value"][0];
		self.displayedElements = self.preferences["veda-ui:displayedElements"][0];
	} catch (e) {
		self.language = defaults.language;
		self.displayedElements = defaults.displayedElements;
	}
	
	self.switch_language = function(language) {
		self.language = language;

		if (language == "RU") {
			self.preferences["veda-ui:preferredLanguage"] = [new IndividualModel(veda, "veda-ui:RU")];
		}
		if (language == "EN") {
			self.preferences["veda-ui:preferredLanguage"] = [new IndividualModel(veda, "veda-ui:EN")];
		}
		self.preferences.save();
		veda.trigger("user:loaded", self);
	}
	
	// Model loaded message
	veda.trigger("user:loaded", self);
		
	return self;
};
