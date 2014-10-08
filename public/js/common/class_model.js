// Class Model

"use strict";

function ClassModel(veda, individual) {

	if (individual instanceof IndividualModel) var self = individual;
	else var self = new IndividualModel(veda, individual);
	
	var documentTemplatesList = query(veda.ticket, "'rdf:type' == 'v-ui:ClassTemplate' && 'v-ui:forClass' == '" + self.id + "'");
	var documentTemplate;
	
	if (documentTemplatesList) {
		Object.defineProperty(self, "documentTemplate", {
			get: function() {
				return documentTemplate ? 
					documentTemplate 
					: 
					documentTemplate = new IndividualModel(veda, documentTemplatesList[0]);
			},
			configurable: true
		});
	}

	self.specs = {};
	var specs = {}; 

	var specsList = query(veda.ticket, "'rdf:type' == 'v-ui:PropertySpecification' && 'v-ui:forClass' == '" + self.id + "'");
	
	if (specsList) {
		specsList.map(function (spec_uri) {
			Object.defineProperty(self.specs, spec_uri, {
				get: function() {
					return specs[spec_uri] ? 
						specs[spec_uri]
						: 
						specs[spec_uri] = new IndividualModel(veda, spec_uri);
				},
				configurable: true
			});
		});
	}

	Object.defineProperty(self, "specsByProps", {
		get: function() {
			return Object.getOwnPropertyNames(self.specs).reduce(function (acc, spec_uri) {
				acc[self.specs[spec_uri]["v-ui:forProperty"][0].id] = self.specs[spec_uri];
				return acc;
			}, {});
		},
		configurable: true
	});
	
	self.domainProperties = function (list) {
		var result = list || {};
		var q = "'rdfs:domain' == '" + self.id + "'";
		var query_result = query(veda.ticket, q);
		query_result.map (function (item) {
			result[item] = result[item] || new IndividualModel(veda, item);
		});
		if (!self["rdfs:subClassOf"]) return result;
		self["rdfs:subClassOf"]
			.filter(function (item) {
				return item instanceof IndividualModel;
			})
			.map( function (item) {
				var superClass = new ClassModel(veda, item);
				superClass.domainProperties(result);
			});
		return result;
	};

	return self;
};
