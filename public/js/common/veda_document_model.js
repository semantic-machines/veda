// Document Model

"use strict";

function DocumentModel(uri) { 
	var self = $.observable(this);

	var individual = {};
	Object.defineProperty(self, "individual", {
		get: function() { return individual; },
		set: function(value) { if (compare(individual, value)) return; individual = value; self.trigger("set", "individual", value); }
    });
	if (console.log) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
		
	self.load = function(uri) {
		get_individual(app.ticket, uri, function(data) {
			self.individual = data;
			self.trigger("loaded");
		});
	};
	self.save = function() {
		put_individual(app.ticket, self.individual, function(data) {
			self.trigger("saved");
		});
	};
	self.on("loaded", function() { 
		console.log("document loaded: ", self.individual);
	});
	self.on("saved", function() { 
		console.log("document saved: ", self.individual);
	});
	if (uri) self.load(uri);
	return self;
};
