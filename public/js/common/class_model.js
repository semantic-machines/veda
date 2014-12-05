// Class Model

;(function (veda) { "use strict";
	
	veda.ClassModel = function (individual) {

		if (individual instanceof veda.IndividualModel) var self = individual;
		else var self = new veda.IndividualModel(individual);
		
		self.domainProperties = self.domainProperties || {};
		
		self.documentTemplate = self.documentTemplate || {};

		self.specsByProps = self.specsByProps || {};
		
		self.subClasses = self.subClasses || {};

		return self;

	};
	
}(veda));
