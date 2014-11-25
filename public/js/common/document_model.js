// Document Model

"use strict";

function DocumentModel(veda, individual, container, template, mode) {

	var self;
	
	if (individual instanceof IndividualModel) self = individual;
	else self = new IndividualModel(veda, individual, true);

	self.off("*");
	
	self.on("individual:loaded individual:reset individual:saved", function (event) {
		veda.trigger("document:loaded", self, container, template);
	});

	/*if (!self["rdf:type"]) {
		document.defineProperty("rdf:type");
		self["rdf:type"] = new IndividualModel(veda, "v-s:Document");
	}*/

	veda.trigger("document:loaded", self, container, template, mode);

	return self;
};
