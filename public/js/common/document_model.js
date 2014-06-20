// Document Model

"use strict";

function DocumentModel(veda, params) {
	var container = params[1];
	
	var self = riot.observable(this);
	
	self.on("individual:loaded", function() {
		veda.trigger("document:loaded", self, container);
	});
	
	// Inherit from IndividualModel
	IndividualModel.call(this, veda, params);
};
