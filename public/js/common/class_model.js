// Class Model

"use strict";

function ClassModel(veda, uri) {
	var self = riot.observable(this);
	
	self.on("individual:loaded", function() {
		veda.trigger("class:loaded", self);
	});
	
	// Inherit from IndividualModel
	IndividualModel.call(self, veda, uri);

	var domainPropertiesList = (function() {
		var q = "'rdfs:domain' == '" + self["@"] + "'" ;
		return query(veda.ticket, q);
	})();

	var domainProperties = {};
	self.domainProperties = {};

	if (domainPropertiesList) 
		domainPropertiesList.map(function (property_uri) {
			Object.defineProperty(self.domainProperties, property_uri, {
				get: function() {
					if (!domainProperties[property_uri]) {
						domainProperties[property_uri] = new IndividualModel(veda, property_uri);
					}
					return domainProperties[property_uri];
				}
			});
		});
	
	var superClasses = {};
	self.superClasses = {};

	if (self["rdfs:subClassOf"]) 
		self["rdfs:subClassOf"].map(function (superClass) {
			Object.defineProperty(self.superClasses, superClass["@"], {
				get: function() {
					return superClasses[superClass["@"]] ? superClasses[superClass["@"]] : superClasses[superClass["@"]] = new ClassModel(veda, [superClass["@"]]);
				}
			});
		});
	
	return self;
};
