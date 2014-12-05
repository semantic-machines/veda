// Property Model

;(function (veda) { "use strict";

	veda.PropertyModel = function (individual) {

		if (individual instanceof veda.IndividualModel) var self = individual;
		else var self = new veda.IndividualModel(individual);
		
		return self;
	};

}(veda));
