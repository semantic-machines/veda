// Class Model

"use strict";

function ClassModel(veda, individual) {

	if (individual instanceof IndividualModel) var self = individual;
	else var self = new IndividualModel(veda, individual);
	
	self.domainProperties = self.domainProperties || {};
	
	self.documentTemplate = self.documentTemplate || {};

	self.specsByProps = self.specsByProps || {};
	
	self.subClasses = self.subClasses || {};

	return self;
};
