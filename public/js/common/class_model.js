// Class Model

"use strict";

function ClassModel(veda, individual) {

	if (individual instanceof IndividualModel) var self = individual;
	else var self = new IndividualModel(veda, individual);

	self.domainProperties = {};
	var domainProperties = {};

	var domainPropertiesList = query(veda.ticket, "'rdfs:domain' == '" + self["@"] + "'");

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
	
	self.templates = {};
	var templates = {}; 
	
	var templatesList = query(veda.ticket, "'veda-ui:forClass' == '" + self["@"] + "'");
	
	if (templatesList) {
		templatesList.map(function (template_uri) {
			Object.defineProperty(self.templates, template_uri, {
				get: function() {
					return templates[template_uri] ? 
						templates[template_uri] 
						: 
						templates[template_uri] = new IndividualModel(veda, template_uri);
				}
			});
		});
	}


	
	return self;
};
