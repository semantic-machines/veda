// Document Model

"use strict";

function DocumentModel(veda, individual, container) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	self.on("individual:loaded", function (_individual, _container) {
		veda.trigger("document:loaded", self, container);
	});
	
	veda.trigger("document:loaded", self, container);

	return self;
};
