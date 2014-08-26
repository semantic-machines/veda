// User Model

"use strict";

function UserModel(veda, individual) {
	var defaults = {
		language : {"RU": veda.availableLanguages["RU"]},
		displayedElements : 10
	};
	
	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);
	
	try { 
		self.preferences = new IndividualModel(veda, self["veda-ui:hasPreferences"][0].id);

		self.language = self.preferences["veda-ui:preferredLanguage"].reduce( function (acc, lang) {
			acc[lang["rdf:value"][0]] = veda.availableLanguages[lang["rdf:value"][0]];
			return acc;
		}, {} );

		self.displayedElements = self.preferences["veda-ui:displayedElements"][0];
	} catch (e) {
		self.language = defaults.language;
		self.displayedElements = defaults.displayedElements;
	}
	
	self.toggleLanguage = function(language_val) {
		
		if (language_val in self.language && Object.keys(self.language).length == 1) return;
				
		language_val in self.language ? delete self.language[language_val] : self.language[language_val] = veda.availableLanguages[language_val];
		
		self.preferences["veda-ui:preferredLanguage"] = Object.keys(self.language).map ( function (language_val) {
			return self.language[language_val];
		});

		self.preferences.save();
		veda.trigger("user:loaded", self);
		veda.trigger("language:changed");
	}
	
	// Model loaded message
	veda.trigger("user:loaded", self);
		
	return self;
};
