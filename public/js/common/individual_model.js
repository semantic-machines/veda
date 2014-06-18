// Document Model

"use strict";

function IndividualModel(veda, params) {
	var uri = params[0];
	var self = riot.observable(this);

	// Define Model functions
	var individual = {};
	self.properties = [];

	self.load = function(uri) {
		individual = veda.cache[uri] ? JSON.parse( veda.cache[uri] ) : get_individual(veda.ticket, uri);
		for (var property_uri in individual) {
			(function(property_uri) {
				Object.defineProperty(self, property_uri, {
					get: function() { 
						if (property_uri == "@") return individual["@"];
						var property = new IndividualModel(veda, [property_uri]);
						var values = individual[property_uri];
						return {property: property, values: values};
					},
					set: function(value) { 
						if (individual[property_uri] == value) return; 
						individual[property_uri] = value; 
						self.trigger("property:changed", property_uri, individual[property_uri]);
					}
				});
				self.properties.push(property_uri);
			})(property_uri);
		}
		self.trigger("individual:loaded");
	};

	self.save = function() {
		put_individual(veda.ticket, individual, function(data) {
		});
	};

	// Load data 
	if (uri) self.load(uri);
	
};
