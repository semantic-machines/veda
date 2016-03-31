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
		
		if ( self.hasValue("v-ui:hasPreferences") ) {
			self.preferences = self["v-ui:hasPreferences"][0];
			if ( !self.preferences.hasValue("v-ui:preferredLanguage") || !self.preferences.hasValue("v-ui:displayedElements")) {
				self.preferences["v-ui:preferredLanguage"] = [ self.availableLanguages["RU"] ];
				self.preferences["v-ui:displayedElements"] = [ 10 ];
				self.preferences.save();
			}
		} else {
			self.preferences = new veda.IndividualModel();
			self.preferences["v-s:author"] = [ self ];
			self.preferences["rdf:type"] = [ new veda.IndividualModel("v-ui:Preferences") ];
			self.preferences["rdfs:label"] = [ "Preferences_" + self.id ];
			self.preferences["v-ui:preferredLanguage"] = [ self.availableLanguages["RU"] ];
			self.preferences["v-ui:displayedElements"] = [ 10 ];
			self.preferences.save();
			self["v-ui:hasPreferences"] = [ self.preferences ];
			self.save();
		}
		self.language = self.preferences["v-ui:preferredLanguage"].reduce( function (acc, lang) {
			acc[lang["rdf:value"][0]] = self.availableLanguages[lang["rdf:value"][0]];
			return acc;
		}, {} );
		self.displayedElements = self.preferences["v-ui:displayedElements"][0];

		if ( self.hasValue("v-s:hasAspect") ) {
			self.aspect = self["v-s:hasAspect"][0];
		} else {
			self.aspect = new veda.IndividualModel();
			self.aspect["rdf:type"] = [ new veda.IndividualModel("v-s:PersonalAspect") ];
			self.aspect["v-s:owner"] = [ self ];
			self.aspect["rdfs:label"] = [ "PersonalAspect_" + self.id ];
			self.aspect.save();
			self["v-s:hasAspect"] = [ self.aspect ];
			self.save();
		}
		
		if (self.hasValue("v-s:defaultAppointment")) {
			veda.appointment = self["v-s:defaultAppointment"][0];
		} else if (self.hasValue("v-s:hasAppointment")) {
			self["v-s:defaultAppointment"] = [ self["v-s:hasAppointment"][0] ];
			self.save();
			veda.appointment = self["v-s:defaultAppointment"][0];
		} else {
			veda.appointment = undefined;
		}

		if (self.hasValue("v-s:hasAppointment")) {
			veda.appointmentList = self["v-s:hasAppointment"].filter( function (item) {
				return item.hasValue("v-s:official") && item["v-s:official"][0] == true;
			});
		}
		self.on("individual:propertyModified", function (property_uri, values) {
			if (property_uri === "v-s:defaultAppointment") {
				self.save();
				veda.appointment = self["v-s:defaultAppointment"][0];
			} 
		});
 
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

		self.toggleLanguage = function(language_val) {
			if (language_val in self.language && Object.keys(self.language).length == 1) return;
			language_val in self.language ? delete self.language[language_val] : self.language[language_val] = self.availableLanguages[language_val];
			self.preferences["v-ui:preferredLanguage"] = Object.keys(self.language).map ( function (language_val) {
				return self.language[language_val];
			});
			self.preferences.save();
		};
			
		return self;
	};

});
