// Document Model

"use strict";

function IndividualModel(veda, uri) {
	var self = riot.observable(this);

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
						if (property_uri == "@") return values[property_uri] = individual["@"];						
						if (values[property_uri]) return values[property_uri];
						values[property_uri] = individual[property_uri].map(function(value) {
							switch (value.type) {
								case "String" : 
									var string = new String(value.data);
									string.language = value.lang;
									return string; 
									break
								case "Uri" : 
									if (value.data.search(/^.{3,5}:\/\//) == 0) return new String(value.data);
									try { return new IndividualModel(veda, value.data); } 
									catch (e) { return new String(value.data) }
									break
								case "Datetime" : return new Date(Number(value.data)); break
								case "Integer" : return new Number(value.data); break
								case "Float" : return new Number(value.data); break
								case "Boolean" : return new Boolean(value.data); break
								default : throw ("Unsupported type of property value"); break
							}
						});
						return values[property_uri];
					},
					set: function(value) { 
						if (values[property_uri] == value) return;
						values[property_uri] = value;
						self.trigger("value:changed", property_uri, values[property_uri]);
					}
				});

				Object.defineProperty(self.properties, property_uri, {
					get: function() { 
						if (properties[property_uri]) return properties[property_uri];
						try { properties[property_uri] = new IndividualModel(veda, property_uri); } 
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
		for (var property_uri in values) {
			if (property_uri == "@") {
				//individual["@"] = values["@"]; 
				continue;
			}
			if (values[property_uri] == undefined) continue;
			individual[property_uri] = values[property_uri].map( function(value) {
				var result = {};
				if (value instanceof Number) {
					//result.type = Number.isInteger(value) ? "Integer" : "Float";
					result.type = "String";
					result.data = value.valueOf();
					return result;
				} else if (value instanceof Boolean) {
					result.type = "Boolean";
					result.data = value.valueOf();
					return result;
				} else if (value instanceof Date) {
					result.type = "String";
					result.data = value.toISOString();
					result.lang = "NONE";
					return result;
				} else if (value instanceof String) {
					result.type = "String";
					result.data = value.valueOf();
					result.lang = value.language || "NONE";
					return result;
				} else if (value instanceof IndividualModel) {
					result.type = "Uri";
					result.data = value["@"];
					return result;
				} else return value;
			});
		}
		console.log(individual);
		put_individual(veda.ticket, individual, function(data) {
		});
	};

	// Load data 
	if (uri) self.load(uri); 
	
	return self;
};
