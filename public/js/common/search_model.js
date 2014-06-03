// Search Model

"use strict";

function SearchModel(veda, params) {
	var self = riot.observable(this);

	// Define Model data setters & getters
	var properties = {q:"", results:"", results_count:""};
	function define_GS_etters(property) {
		(function(property) {
			Object.defineProperty(self, property, {
				get: function() { return properties[property]; },
				set: function(value) { 
					if (properties[property] == value) return; 
					properties[property] = value; 
					self.trigger("property:changed", property, properties[property]);
				}
   			});
   		})(property);
	};
	for (var property in properties) {
		define_GS_etters(property);
    }

	// Define Model functions
	self.search = function() {
		query(veda.ticket, self.q, function(data) {
			self.results = data;
			self.results_count = data.length;
			self.trigger("search:complete");
		});
	};

	self.load_results = function(container) {
		for (var i in self.results) {
			new DocumentModel(veda, [self.results[i], container]);
		}
	};
	
	// Model messages
	veda.trigger("search:loaded", self);
	
	// Search if params given
	self.q = params[0];
	if (self.q) self.search();
	
};
