// Class Model

"use strict";

function ClassModel(veda, individual) {

	if (individual instanceof IndividualModel) var self = individual;
	else var self = new IndividualModel(veda, individual);

	self.domainProperties = {};
	var domainProperties = {};

	var domainPropertiesList = query(veda.ticket, "'rdfs:domain' == '" + self.id + "'");

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
	
	var documentTemplatesList = query(veda.ticket, "'rdf:type' == 'veda-ui:DocumentTemplate' && 'veda-ui:forClass' == '" + self.id + "'");
	var documentTemplate;
	
	if (documentTemplatesList) {
		Object.defineProperty(self, "documentTemplate", {
			get: function() {
				return documentTemplate ? 
					documentTemplate 
					: 
					documentTemplate = new IndividualModel(veda, documentTemplatesList.pop());
			}
		});
	}

	self.specs = {};
	var specs = {}; 

	var specsList = query(veda.ticket, "'rdf:type' == 'veda-ui:PropertySpecification' && 'veda-ui:forClass' == '" + self.id + "'");
	
	if (specsList) {
		specsList.map(function (spec_uri) {
			Object.defineProperty(self.specs, spec_uri, {
				get: function() {
					return specs[spec_uri] ? 
						specs[spec_uri]
						: 
						specs[spec_uri] = new IndividualModel(veda, spec_uri);
				}
			});
		});
	}

	Object.defineProperty(self, "specsByProps", {
		get: function() {
			return Object.getOwnPropertyNames(self.specs).reduce(function (acc, spec_uri) {
				acc[self.specs[spec_uri]["veda-ui:forProperty"][0].id] = self.specs[spec_uri];
				return acc;
			}, {});
		}
	});
	
	return self;
};
