// Document Model

"use strict";

function DocumentModel(veda, uri, container) {
	var self = riot.observable(this);
	
	self.on("individual:loaded", function() {
		veda.trigger("document:loaded", self, container);
	});
	
	// Inherit from IndividualModel
	IndividualModel.call(self, veda, uri);
	
	return self;
};
