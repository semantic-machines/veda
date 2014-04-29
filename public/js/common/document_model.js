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
		
		function localize(resources, lang) {
			var localed = [], unlocaled = [];
			for (var i in resources) {
				if ( resources[i]["lang"] == lang) localed.push(resources[i]["data"]);
				if (typeof resources[i]["lang"] == "undefined") unlocaled.push(resources[i]["data"]);
			}
			return localed.length ? localed : unlocaled;
		}

		var flat_individual = {}, individual = self.individual;
		for (var property_uri in individual) {
			if (property_uri == "@") {
				flat_individual["@"] = individual["@"];
				continue;
			}
			var property = veda.cache[property_uri] ? JSON.parse( veda.cache[property_uri] ) : get_individual(veda.ticket, property_uri);
			var property_label = localize( property["rdfs:label"], veda.user.language )[0];
			var property_range = property["rdfs:range"];
			
			var values = localize(individual[property_uri], veda.user.language);
			flat_individual[property_uri] = {property_uri: property_uri, property_label: property_label, property_range:property_range, property_values: values};
		}
		self.flat_individual = flat_individual; 
		

		// TODO: move individual to separate model (and perhaps presenter?)

		self.expanded = {};
		var ind = self.individual;
		for (property_uri in ind) {
			self.expanded[property_uri] = {};
			if (property_uri == "@") {
				self.expanded["@"] = ind["@"];
				continue;
			}
			var property = veda.cache[property_uri] ? JSON.parse( veda.cache[property_uri] ) : get_individual(veda.ticket, property_uri);
			var values = ind[property_uri];
			for (var i in values) {
				if (values[i].type != "Uri") continue;
				values[i] = veda.cache[values[i].data] ? JSON.parse( veda.cache[values[i].data] ) : get_individual(veda.ticket, values[i].data);
			}
			self.expanded[property_uri]["property"] = property;
			self.expanded[property_uri]["values"] = values;
		}
		
		function localize_values(values) {
			var localed = [], unlocaled = [], lang = veda.user.language;
			for (var i in values) {
				if (values[i]["lang"] == lang) localed.push(values[i]);
				if (typeof values[i]["lang"] == "undefined") unlocaled.push(values[i]);
			}
			values = localed.length ? localed : unlocaled;
			return values;
		}
		
		function localize_individual(individual) {
			for (var i in individual) {
				if (i == "@") continue;
				individual[i] = localize_values(individual[i]);
			}
			return individual;
		}
		
		var localized_individual = localize_individual(self.individual);

		var localized_expanded = self.expanded;
		for (var i in localized_expanded) {
			if (i == "@") continue;
			localized_expanded[i].property = localize_individual(localized_expanded[i].property);
			for (var j in localized_expanded[i].values) {
				if (!localized_expanded[i].values[j]["@"]) continue;
				localized_expanded[i].values[j] = localize_individual(localized_expanded[i].values[j]);
			}
		}
		console.log(localized_expanded);

		
		self.trigger("document:loaded");
	};

	self.save = function() {
		put_individual(veda.ticket, self.individual, function(data) {
		});
	};

	// Model loaded message
	self.on("document:loaded", function() {
		if (veda) veda.trigger("document:loaded", self);
	});

	// Load data 
	if (uri) self.load(uri);
	
};
