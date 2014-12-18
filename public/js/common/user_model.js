// User Model

;(function (veda) { "use strict";

	veda.UserModel = function (individual) {
		
		var defaults = {
			language : {"RU": veda.availableLanguages["RU"]},
			displayedElements : 10
		};
		
		var self = new veda.IndividualModel(individual);
		
		try { 
			self.preferences = self["v-ui:hasPreferences"][0];

			self.language = self.preferences["v-ui:preferredLanguage"].reduce( function (acc, lang) {
				acc[lang["rdf:value"][0]] = veda.availableLanguages[lang["rdf:value"][0]];
				return acc;
			}, {} );

			self.displayedElements = self.preferences["v-ui:displayedElements"][0];
		} catch (e) {
			self.language = defaults.language;
			self.displayedElements = defaults.displayedElements;
		}
		
		self.preferences.on("individual:propertyModified:v-ui:displayedElements", function (values) {
			self.displayedElements = values[0];
		});

		self.preferences.on("individual:propertyModified:v-ui:preferredLanguage", function (values) {
			self.language = values.reduce( function (acc, lang) {
				acc[lang["rdf:value"][0]] = veda.availableLanguages[lang["rdf:value"][0]];
				return acc;
			}, {} );
		});
		
		self.toggleLanguage = function(language_val) {
			
			if (language_val in self.language && Object.keys(self.language).length == 1) return;
					
			language_val in self.language ? delete self.language[language_val] : self.language[language_val] = veda.availableLanguages[language_val];
			
			self.preferences["v-ui:preferredLanguage"] = Object.keys(self.language).map ( function (language_val) {
				return self.language[language_val];
			});

			self.preferences.save();
			veda.trigger("language:changed");
		}
			
		return self;
	};

})(veda);
