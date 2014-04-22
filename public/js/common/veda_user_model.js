// User Model

"use strict";

VedaModel.prototype.UserModel = function UserModel(uri) {
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
		get_individual(app.ticket, uri, function(data) {
			self.individual = data;
			self.trigger("user:loaded");
		});
	};
	self.save = function() {
		put_individual(app.ticket, self.individual, function(data) {
			self.trigger("user:saved");
		});
	};

	// Load data 
	if (uri) self.load(uri);
};
