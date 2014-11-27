// Document Model

"use strict";

function DocumentModel(veda, individual, container, template, mode) {

	var self;
	
	if (individual instanceof IndividualModel) self = individual;
	else self = new IndividualModel(veda, individual, true);

	self.off("*");
	
	self.on("individual:loaded individual:reset", function (event) {
		veda.trigger("document:loaded", self, container, template);
	});

	veda.trigger("document:loaded", self, container, template, mode);

	return self;
};
