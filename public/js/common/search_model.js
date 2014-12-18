// Search Model

;(function (veda) { "use strict";

	veda.SearchModel = function (q, container) {
		var self = riot.observable(this);

		// Define Model data setters & getters
		var properties = {q:undefined, results:{}, results_count:undefined, selected:{}};
		for (var property in properties) {
			(function (property) {
				Object.defineProperty(self, property, {
					get: function () { return properties[property]; },
					set: function (value) { 
						if (properties[property] == value) return; 
						properties[property] = value; 
						self.trigger("property:changed", property, properties[property]);
					}
				});
			})(property);
		}

		self.toggleSelected = function (i) {
			if (!self.results[i]) return self.selected;
			if (self.results[i].id in self.selected) {
				delete self.selected[self.results[i].id];
			} else {
				self.selected[self.results[i].id] = self.results[i];
			}
			self.trigger("search:selected", self.results[i], self.selected);
			return self.selected;
		}

		self.toggleAll = function () {
			if (Object.keys(self.selected).length != self.results_count) {
				for (var i=0; i < self.results_count; i++) {
					self.selected[self.results[i].id] = self.results[i];
				}
			} else {
				self.selected = {};
			}
			console.log(self.selected);
			return self.selected;
		}

		// Define Model functions
		self.search = function () {
			
			// Clear previous results 
			self.results = {};
			
			var results = query(veda.ticket, self.q);
			for (var i in results) {
				(function(i){
					Object.defineProperty(self.results, i, {
						get: function () { 
							if (typeof results[i] == 'object') return results[i];
							return results[i] = new veda.SearchResultModel(results[i]);
						}
					});
				})(i);
			}
			self.results_count = results.length;
			self.trigger("search:complete");

		};

		// Model messages
		self.on("search:complete", function () {
			veda.trigger("search:complete", self, container);
		});
		
		veda.trigger("search:loaded", self, container);
		
		// Search if params given
		self.q = q;
		if (self.q) self.search();
		
		return self;
	};

})(veda);
