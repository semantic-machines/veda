// Document Model

"use strict";

function IndividualModel(veda, params) {
	var self = riot.observable(this);
	var uri = params[0];

	// Define Model functions
	var individual = {};
	var properties = {};
	var values = {};
	self.properties = {};

	self.load = function(uri) {
		individual = veda.cache[uri] ? JSON.parse( veda.cache[uri] ) : get_individual(veda.ticket, uri);
		for (var property_uri in individual) {
			(function(property_uri) {
				
				properties[property_uri] = undefined;
				values[property_uri] = undefined;

				Object.defineProperty(self, property_uri, {
					get: function() { 
						if (property_uri == "@") return individual["@"];
						if (values[property_uri]) return values[property_uri];
						values[property_uri] = individual[property_uri].map(function(value) {
							switch (value.type) {
								case "Uri" : 
									if (value.data.search(/^.{3,5}:\/\//) == 0) return String(value.data);
									try { return new IndividualModel(veda, [value.data]); } 
									catch (e) { return String(value.data); }
								case "String" : 
									if (value.lang != veda.user.language && value.lang != 'NONE') return undefined;
									return String(value.data); break
								case "Integer" : return Number(value.data); break
								case "Datetime" : return Date(Number(value.data)); break
								case "Date" : return Date( Number(value.data) ); break
								case "Float" : return Number(value.data); break
								case "Boolean" : return Boolean(value.data); break
								default : return; break
							}
						// Remove "null" & "undefined" values
						}).filter(function(item){return item}); 
						return values[property_uri];
					},
					set: function(value) { 
						if (individual[property_uri] == value) return;
						individual[property_uri] = value;
						self.trigger("property:changed", property_uri, individual[property_uri]);
					}
				});

				Object.defineProperty(self.properties, property_uri, {
					get: function() { 
						if (properties[property_uri]) return properties[property_uri];
						try { properties[property_uri] = new IndividualModel(veda, [property_uri]); } 
						catch (e) { properties[property_uri] = property_uri; }
						return properties[property_uri];
					},
					set: function(value) { 
						if (properties[property_uri] == value) return; 
						properties[property_uri] = value; 
						self.trigger("property:changed", property_uri, properties[property_uri]);
					}
				});
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
