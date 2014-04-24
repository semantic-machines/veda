// Document Model

"use strict";

function DocumentModel(veda, params) {
	var uri = params[0];
	var self = riot.observable(this);

	// Define Model data setters & getters
	var _ = {individual:""};
	for (var property in _) {
		(function(property) {
			Object.defineProperty(self, property, {
				get: function() { return _[property]; },
				set: function(value) { if (compare(_[property], value)) return; _[property] = value; self.trigger("set", property, _[property]); }
   			});
   		})(property);
    }
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
		
	// Define Model functions
	self.load = function(uri) {
		get_individual(veda.ticket, uri, function(data) {
			self.individual = data;
		});
	};
	self.save = function() {
		put_individual(veda.ticket, self.individual, function(data) {
		});
	};

	// Load data 
	if (uri) self.load(uri);

	// Model loaded message
	if (veda) veda.trigger("document:loaded", self);
};
