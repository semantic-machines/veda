// Search Model

"use strict";

function SearchModel(veda, params) {
	var self = riot.observable(this);

	// Define Model data setters & getters
	
	// Model loaded message
	self.on("search:loaded", function() {
		if (veda) veda.trigger("search:loaded", self);
	});
};
