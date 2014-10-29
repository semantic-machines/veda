// Class Model

"use strict";

function PropertyModel(veda, individual) {

	if (individual instanceof IndividualModel) var self = individual;
	else var self = new IndividualModel(veda, individual);
	
	return self;
};
