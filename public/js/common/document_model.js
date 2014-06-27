// Document Model

"use strict";

function DocumentModel(veda, params) {
	var container = params[1];
	
	var self = riot.observable(this);
	
	self.on("individual:loaded", function() {
		veda.trigger("document:loaded", self, container);
	});
	
	// Inherit from IndividualModel
	IndividualModel.call(self, veda, params);

	/*Object.defineProperty(self, "domainProperties", {
		get: function() {
			
			var types = self["rdf:type"].map( function(item) {
				
				var query = "'rdfs:domain' == '" + item["@"] + "'" ;
			})
			
		},
		set: function(value) { 
		}
	});*/
	
};
