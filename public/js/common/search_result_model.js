// Document Model

"use strict";

function SearchResultModel(veda, individual, container, template) {

	var self = individual instanceof IndividualModel ? individual : new IndividualModel(veda, individual);

	self.off("*");
	
	self.on("individual:loaded individual:reset individual:saved", function (event) {
		veda.trigger("search_result:loaded", self, container, template);
	});
	
	veda.trigger("search_result:loaded", self, container, template);

	return self;
};
