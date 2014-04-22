// Search Model

"use strict";

function SearchModel(veda, params) {
	var self = riot.observable(this);

	// Define Model data setters & getters
	var variable = {};
	Object.defineProperty(self, "variable", {
		get: function() { return variable; },
		set: function(value) { if (compare(variable, value)) return; variable = value; self.trigger("set", "variable", value); }
    });
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
	
	// Model loaded message
	if (veda) veda.trigger("search:loaded", self);
};
