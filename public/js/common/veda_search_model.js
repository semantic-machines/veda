// Search Model

"use strict";

VedaModel.prototype.SearchModel = function SearchModel() {
	var self = riot.observable(this);

	// Define Model data setters & getters
	var variable = {};
	Object.defineProperty(self, "variable", {
		get: function() { return variable; },
		set: function(value) { if (compare(variable, value)) return; variable = value; self.trigger("set", "variable", value); }
    });
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });
};
