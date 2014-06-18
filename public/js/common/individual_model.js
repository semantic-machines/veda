// Document Model

"use strict";

function IndividualModel(veda, params) {
	var uri = params[0];
	var self = riot.observable(this);

	// Define Model functions
	var individual = {};
	self.predicates = {};
	self.objects = {};

	self.load = function(uri) {
		individual = veda.cache[uri] ? JSON.parse( veda.cache[uri] ) : get_individual(veda.ticket, uri);
		for (var property_uri in individual) {
			(function(property_uri) {
				Object.defineProperty(self, property_uri, {
					get: function() { 
						if (property_uri == "@") return individual["@"];
						if (!self.predicates[property_uri]) self.predicates[property_uri] = new IndividualModel(veda, [property_uri]); 
						/*self.objects[property_uri] = individual[property_uri].map( function(resource) {
							resource.data = resource.type == "Uri" ? new IndividualModel(veda, [resource.data]) : resource.data;
							return resource;
						});*/
						return individual[property_uri]; 
					},
					set: function(value) { 
						if (individual[property_uri] == value) return; 
						individual[property_uri] = value; 
						self.trigger("property:changed", property_uri, individual[property_uri]);
					}
				});
				self.predicates[property_uri] = undefined;
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
