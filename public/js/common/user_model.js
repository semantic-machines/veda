// User Model

"use strict";

function UserModel(veda, individual) {
	var defaults = {
		language : {"veda-ui:RU": veda.availableLanguages["veda-ui:RU"]},
		displayedElements : 10
	};
	
	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);
	
	try { 
		self.preferences = new IndividualModel(veda, self["veda-ui:hasPreferences"][0].id);

		self.language = self.preferences["veda-ui:preferredLanguage"].reduce( function (acc, lang) {
			acc[lang.id] = veda.availableLanguages[lang.id];
			return acc;
		}, {} );

		self.displayedElements = self.preferences["veda-ui:displayedElements"][0];
	} catch (e) {
		self.language = defaults.language;
		self.displayedElements = defaults.displayedElements;
	}
	
	self.toggleLanguage = function(language_uri) {
		
		if (language_uri in self.language && Object.keys(self.language).length == 1) return;
				
		language_uri in self.language ? delete self.language[language_uri] : self.language[language_uri] = veda.availableLanguages[language_uri];
		
		self.preferences["veda-ui:preferredLanguage"] = Object.keys(self.language).map ( function (language_uri) {
			return self.language[language_uri];
		});

		self.preferences.save();
		veda.trigger("user:loaded", self);
		veda.trigger("language:changed");
	}
	
	// Model loaded message
	veda.trigger("user:loaded", self);
		
	return self;
};
