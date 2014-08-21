// Document Model

"use strict";

function IndividualModel(veda, uri, container) {
	var self = riot.observable(this);

	// Define Model functions
	var individual = {};
	var properties = {};
	var values = {};
	self.properties = {};
	self.propertiesList = [];

	if (!uri) individual["@"] = guid();
	
	Object.defineProperty(self, "id", {
		get: function () { 
			return individual["@"];
		},
		set: function (value) { 
			individual["@"] = value;
		}
	});

	self.defineProperty = function (property_uri) {

		properties[property_uri] = undefined;
		values[property_uri] = undefined;

		Object.defineProperty(self, property_uri, {
			get: function () { 
				if (values[property_uri]) return values[property_uri];
				values[property_uri] = individual[property_uri].map( function (value) {
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
						case "Datetime" : return new Date(Date.parse(value.data)); break
						case "Decimal" : return new Number(value.data); break
						case "Integer" : return new Number(value.data); break
						case "Boolean" : return new Boolean(value.data); break
						default : throw ("Unsupported type of property value"); break
					}
				});
				return values[property_uri].filter(function( item ){ return item }); // Remove undefined values
			},
			set: function (value) { 
				if (values[property_uri] == value) return;
				values[property_uri] = value;
				individual[property_uri] = values[property_uri].map( function (value) {
					var result = {};
					if (value instanceof Number || typeof value === "number" ) {
						result.type = Number.isInteger(value.valueOf()) ? "Integer" : "Decimal";
						result.data = value.valueOf();
						return result;
					} else if (value instanceof Boolean || typeof value === "boolean") {
						result.type = "Boolean";
						result.data = value.valueOf();
						return result;
					} else if (value instanceof String || typeof value === "string") {
						result.type = "String";
						result.data = value.valueOf();
						result.lang = value.language || "NONE";
						return result;
					} else if (value instanceof Date) {
						result.type = "Datetime";
						result.data = value.toISOString();
						result.lang = "NONE";
						return result;
					} else if (value instanceof IndividualModel) {
						result.type = "Uri";
						result.data = value.id;
						return result;
					} else {
						return value;
					}
				});
				self.trigger("value:changed", property_uri, value);
			}
		});
		
		Object.defineProperty(self.properties, property_uri, {
			get: function () { 
				if (properties[property_uri]) return properties[property_uri];
				try { properties[property_uri] = new IndividualModel(veda, property_uri); } 
				catch (e) { properties[property_uri] = property_uri; }
				return properties[property_uri];
			},
			set: function (value) { 
				if (properties[property_uri] == value) return; 
				properties[property_uri] = value; 
				self.trigger("property:changed", property_uri, value);
			}
		});
		
	}

	self.load = function (uri) {
		individual = veda.cache[uri] ? JSON.parse( veda.cache[uri] ) : get_individual(veda.ticket, uri);
		Object.keys(individual).map(function (property_uri) {
			if (property_uri == "@") return;
			self.defineProperty(property_uri);
		});
		self.trigger("individual:loaded", self, container);
	};

	self.save = function() {
		put_individual(veda.ticket, individual, function (data) {
			self.trigger("individual:saved", self, container);
		});
	};

	self.on("individual:saved individual:loaded", function ( event ) {
		veda.trigger(event, self, container);
	});

	// Load data 
	if (uri) self.load(uri); 
	
	return self;
};
