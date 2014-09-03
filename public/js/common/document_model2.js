// Document Model

"use strict";

function DocumentModel(veda, individual, container) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	veda.trigger("document:loaded", self, container);
	
	return self;
};
