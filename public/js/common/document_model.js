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

		// TODO: move individual to separate model (and perhaps presenter?)

		self.expanded = {};
		var ind = self.individual;
		for (var property_uri in ind) {
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
		
		self.expanded_localized = JSON.parse(
			JSON.stringify( self.expanded )
			.replace(/{[^{]*?"lang":"(.{2})"[^}]*?}/g, function (match, lang) {
				return lang == veda.user.language ? match : "";
			})
			.replace(/\[(,)|(,)\]|\[(,)\]/g, function (match, p1, p2, p3) {
				return p1 ? "[" : p2 ? "]" : "[]";
			})
		);
		
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
