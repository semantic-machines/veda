// User Model

"use strict";

function UserModel(veda, params) {
	var uri = params[0];
	var defaultLanguage = "RU";
	var self = riot.observable(this);

	// Define Model data setters & getters
	var _ = {individual:"", language:"", preferences: ""};
	function define_GS_etters() {
		
	};
	for (var property in _) {
		(function(property) {
			Object.defineProperty(self, property, {
				get: function() { return _[property]; },
				set: function(value) { if (compare(_[property], value)) return; _[property] = value; self.trigger("set", property, _[property]); }
   			});
   		})(property);
    }
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
		
	// Define Model functions
	self.load = function(uri) {
		get_individual(veda.ticket, uri, function(individual) {
			self.individual = individual;
			try { 
				self.preferences = get_property_chain(veda.ticket, self.individual, "veda-ui:hasPreferences").last;
				self.language = get_property_chain(veda.ticket, self.preferences, "veda-ui:preferredLanguage", "rdf:value").field[0].data;
			} catch (e) {
				self.language = defaultLanguage;
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

	// Load data 
	if (uri) self.load(uri);
	
	// Model loaded message
	self.on("user:loaded", function(){
		if (veda) veda.trigger("user:loaded", self);
	});
};
