// Class Model

"use strict";

function ClassModel(veda, individual) {

	if (individual instanceof IndividualModel) var self = individual;
	else var self = new IndividualModel(veda, individual);

	var domainPropertiesList = query(veda.ticket, "'rdfs:domain' == '" + self["@"] + "'");

	self.domainProperties = {};
	var domainProperties = {};

	if (domainPropertiesList) {
		domainPropertiesList.map(function (property_uri) {
			Object.defineProperty(self.domainProperties, property_uri, {
				get: function() {
					return domainProperties[property_uri] ? 
						domainProperties[property_uri] 
						: 
						domainProperties[property_uri] = new IndividualModel(veda, property_uri);
				}
			});
		});
	}
	
	return self;
};
