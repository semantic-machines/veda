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
	//if (typeof console != "undefined") self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
		
	// Define Model functions
	self.load = function(uri) {
		self.individual = get_individual(veda.ticket, uri);
		self.trigger("document:loaded");
	};

	self.save = function() {
		put_individual(veda.ticket, self.individual, function(data) {
		});
	};

	self.get_expanded = function () {
		var expanded = {};
		for (var property_uri in self.individual) {
			expanded[property_uri] = {};
			if (property_uri == "@") {
				expanded["@"] = self.individual["@"];
				continue;
			}
			var property = veda.cache[property_uri] ? JSON.parse( veda.cache[property_uri] ) : get_individual(veda.ticket, property_uri);
			var values = self.individual[property_uri];
			for (var i in values) {
				if (values[i].type != "Uri") continue;
				if (values[i].data.indexOf("://") >= 0) continue; // Link to external resource
				values[i] = veda.cache[values[i].data] ? JSON.parse( veda.cache[values[i].data] ) : get_individual(veda.ticket, values[i].data);
			}
			expanded[property_uri]["property"] = property;
			expanded[property_uri]["values"] = values;
		}
		return expanded;
	}
	
	// Model loaded message
	self.on("document:loaded", function() {
		if (veda) veda.trigger("document:loaded", self);
	});

	// Load data 
	if (uri) self.load(uri);
	
};
