// User Model

"use strict";

function UserModel(veda, params) {
	var uri = params[0];
	var defaults = {
		language : "RU",
		displayedElements : 10
	};
	var self = riot.observable(this);

	// Define Model data setters & getters
	var properties = {individual:"", language:"", preferences: ""};
	function define_GS_etters(property) {
		(function(property) {
			Object.defineProperty(self, property, {
				get: function() { return properties[property]; },
				set: function(value) { 
					if (properties[property] == value) return; 
					properties[property] = value; 
					self.trigger("property:changed", property, properties[property]);
				}
   			});
   		})(property);
	};
	for (var property in properties) {
		define_GS_etters(property);
    }
		
	// Define Model functions
	self.load = function(uri) {
		get_individual(veda.ticket, uri, function(individual) {
			self.individual = individual;
			try { 
				self.preferences = get_property_chain(veda.ticket, self.individual, "veda-ui:hasPreferences").last;
				self.language = get_property_chain(veda.ticket, self.preferences, "veda-ui:preferredLanguage", "rdf:value").field[0].data;
				self.displayedElements = self.preferences["veda-ui:displayedElements"][0].data;
			} catch (e) {
				self.language = defaults.language;
				self.displayedElements = defaults.displayedElements;
				console.log("User preferred language undefined! Default applied.");
			}
			self.trigger("user:loaded");
		});
	};
	self.save = function() {
		put_individual(veda.ticket, self.individual, function(data) {
			self.trigger("user:saved");
		});
	};

	// Model loaded message
	self.on("user:loaded", function(){
		if (veda) veda.trigger("user:loaded", self);
	});
	
	// Load data 
	if (uri) self.load(uri);

};
