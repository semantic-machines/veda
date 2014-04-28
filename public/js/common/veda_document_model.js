// Document Model

"use strict";

function DocumentModel(veda, params) {
	var uri = params[0];
	var self = riot.observable(this);

	// Define Model data setters & getters
	var properties = {individual:""};
	function define_GS_etters(property) {
		(function(property) {
			Object.defineProperty(self, property, {
				get: function() { return properties[property]; },
				set: function(value) { 
					if (properties[property] == value) return; 
					properties[property] = value; 
					self.trigger("set", property, properties[property]);
				}
   			});
   		})(property);
	};
	for (var property in properties) {
		define_GS_etters(property);
    }
	if (typeof console != "undefined") self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
		
	// Define Model functions
	self.load = function(uri) {
		self.individual = get_individual(veda.ticket, uri);
		
		/*
		function localize_resources(resources, lang) {
			var localed = [], unlocaled = [];
			for (var i in resources) {
				if ( resources[i]["lang"] == lang) localed.push(resources[i]["data"]);
				if (typeof resources[i]["lang"] == "undefined") unlocaled.push(resources[i]["data"]);
			}
			return localed.length ? localed : unlocaled;
		}
		var flat_individual = {};
		for (var property_uri in individual) {
			if (property_uri == "@") {
				flat_individual["@"] = individual["@"];
				continue;
			}
			var property_name = localize_resources( get_individual(veda.ticket, property_uri)["rdfs:label"], veda.user.language );
			var values = localize_resources(individual[property_uri], veda.user.language);
			flat_individual[property_name] = values;
		}
		*/
		
		
		self.trigger("document:loaded");
	};

	self.save = function() {
		put_individual(veda.ticket, self.individual, function(data) {
		});
	};

	// Model loaded message
	self.on("document:loaded", function(){
		if (veda) veda.trigger("document:loaded", self);
	});

	// Load data 
	if (uri) self.load(uri);
	
};
