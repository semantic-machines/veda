// User Model

"use strict";

function UserModel(veda, params) {
	var uri = params[0];
	var self = riot.observable(this);

	// Define Model data setters & getters
	var individual = {};
	Object.defineProperty(self, "individual", {
		get: function() { return individual; },
		set: function(value) { if (compare(individual, value)) return; individual = value; self.trigger("set", "individual", value); }
    });
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
		
	// Define Model functions
	self.load = function(uri) {
		get_individual(veda.ticket, uri, function(data) {
			individual = data;
			self.trigger("user:loaded");
		});
	};
	self.save = function() {
		put_individual(veda.ticket, self.individual, function(data) {
			self.trigger("user:saved");
		});
	};

	// Load data 
	if (uri) self.load(uri);
	
	// Model loaded message
	self.on("user:loaded", function(){
		if (veda) veda.trigger("user:loaded", self);
	});
};
