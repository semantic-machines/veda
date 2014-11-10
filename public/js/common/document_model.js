// Document Model

"use strict";

function DocumentModel(veda, individual, container, template) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	self.off("*");
	
	self.on("individual:loaded individual:reset individual:saved", function (event) {
		veda.trigger("document:loaded", self, container, template);
	});

	veda.trigger("document:loaded", self, container, template);

	return self;
};
