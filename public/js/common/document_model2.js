// Document Model

"use strict";

function DocumentModel2(veda, individual, container) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	veda.trigger("document2:loaded", self, container);
	
	return self;
};
