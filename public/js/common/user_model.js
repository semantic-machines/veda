// User Model

veda.Module(function (veda) { "use strict";

	veda.UserModel = function (uri) {
		
		var self = new veda.IndividualModel(uri);
		
		var langs = query(veda.ticket, "'rdf:type' == 'v-ui:Language'");
		self.availableLanguages = langs.reduce ( 
			function (acc, language_uri) {
				var lang = new veda.IndividualModel(language_uri);
				acc[lang["rdf:value"][0]] = lang;  
				return acc;
			}, {});
		
		var defaults = {
			language : {"RU": self.availableLanguages["RU"]},
			displayedElements : 10
		};
		
		if ( self.hasValue("v-asp:hasAspect") ) {
			self.aspect = self["v-asp:hasAspect"][0];
		} else {
			self.aspect = new veda.IndividualModel();
			self.aspect["rdf:type"] = [ veda.ontology["v-asp:PersonalAspect"] ];
			self.aspect.save();
			self["v-asp:hasAspect"] = [self.aspect];
			self.save();
		}
		
		try { 
			self.preferences = self["v-ui:hasPreferences"][0];

			self.language = self.preferences["v-ui:preferredLanguage"].reduce( function (acc, lang) {
				acc[lang["rdf:value"][0]] = self.availableLanguages[lang["rdf:value"][0]];
				return acc;
			}, {} );

			self.displayedElements = self.preferences["v-ui:displayedElements"][0];
		} catch (e) {
			self.language = defaults.language;
			self.displayedElements = defaults.displayedElements;
		}

		if (self.preferences) { 
			self.preferences.on("individual:propertyModified", function (property_uri, values) {
				if (property_uri === "v-ui:displayedElements") {
					self.displayedElements = values[0];
				} 
				if (property_uri === "v-ui:preferredLanguage") {
					self.language = values.reduce( function (acc, lang) {
						acc[lang["rdf:value"][0]] = self.availableLanguages[lang["rdf:value"][0]];
						return acc;
					}, {} );
				}
			});
		}
		
		self.toggleLanguage = function(language_val) {
			
			if (language_val in self.language && Object.keys(self.language).length == 1) return;
					
			language_val in self.language ? delete self.language[language_val] : self.language[language_val] = self.availableLanguages[language_val];
			
			self.preferences["v-ui:preferredLanguage"] = Object.keys(self.language).map ( function (language_val) {
				return self.language[language_val];
			});

			self.preferences.save();
			veda.trigger("language:changed");
		};
			
		return self;
	};

});
