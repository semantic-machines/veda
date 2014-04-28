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
		
		
		var flat = {}, individual = self.individual;
		for (var property in individual) {
			if (property == "@") {
				flat[property] = individual[property];
				continue;
			}
			var property_labels = get_individual(veda.ticket, property)["rdfs:label"];
			var property_label, not_localized;
			for (var index in property_labels) {
				property_label = property_labels[index];
				if (property_label.lang == veda.user.language) break;
				if (!property_label.lang || property_label.lang == "NONE") not_localized = property_label.data;
			}
			if ( property_label.lang != veda.user.language ) property_label.data = not_localized;
			
			var property_values = individual[property];
			var sum_value = "", not_localized = "";
			for (var index in property_values) {
				if (property_values[index].type == "String") {
					if (!property_values[index].lang || property_label.lang == "NONE") not_localized += property_values[index].data + ( index == property_values.length-1 ? "" : ", ");
					if (property_values[index].lang != veda.user.language) continue;
				}
				sum_value += property_values[index].data + (index == property_values.length-1 ? "" : ", ");
			}
			if (!sum_value) sum_value = not_localized;
			flat[property_label.data] = sum_value;
		}
		self.flat_individual = flat;
	
		
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
