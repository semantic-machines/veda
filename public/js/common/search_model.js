// Search Model

veda.Module(function SearchModel(veda) { "use strict";

	veda.SearchModel = function (q, container, queryPrefix) {
		var self = riot.observable(this);
		var results_keys;
		
		self.id = guid(); 
		
		// Define Model data setters & getters
		var properties = {q:"", queryPrefix:"", types:[], sort:"", results:{}, results_count:undefined, selected:{}, query_time:undefined};
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
			if (!self.results[ results_keys[i] ]) return self.selected;
			if (self.results[ results_keys[i] ].id in self.selected) {
				delete self.selected[self.results[ results_keys[i] ].id];
			} else {
				self.selected[self.results[ results_keys[i] ].id] = self.results[ results_keys[i] ];
			}
			self.trigger("search:selected", self.results[ results_keys[i] ], self.selected);
			return self.selected;
		}

		self.toggleAll = function () {
			if (Object.keys(self.selected).length != self.results_count) {
				for (var i=0; i < self.results_count; i++) {
					self.selected[self.results[ results_keys[i] ].id] = self.results[ results_keys[i] ];
				}
			} else {
				self.selected = {};
			}
			//console.log(self.selected);
			return self.selected;
		}

		// Define Model functions
		self.search = function (qq) {
			self.q = qq || self.q;
			
			// Clear previous results 
			self.results = {};
			var t1 = Date.now();
			q = self.q;

			// Transform user input like "roman karpov" to "'*'=='roman' && '*'=='karpov'"
			if (q && q.indexOf("==") < 0) {
				q = q.trim().split(" ").map(function (t) { return "'*'=='" + t + "*'"}).join("&&");
			}
			
			// Prefix query if defined in constructor
			q = (self.queryPrefix ? self.queryPrefix + "&&" : "") + (q ? q : "") ;
			
			var results = query(veda.ticket, q, self.sort);
						
			var t2 = Date.now();
			self.query_time = t2 - t1;
			for (var i in results) {
				(function(i){
					Object.defineProperty(self.results, results[i], {
						get: function () { 
							if (typeof results[i] == 'object') return results[i];
							return results[i] = new veda.IndividualModel(results[i]);
						},
						enumerable: true
					});
				})(i);
			}
			self.results_count = results.length;
			results_keys = Object.getOwnPropertyNames(self.results);
			self.trigger("search:complete");

		};

		// Model messages
		self.on("search:complete", function () {
			veda.trigger("search:complete", self, container);
		});
		
		self.on("search:loaded", function () {
			veda.trigger("search:loaded", self, container);
		});
		
		self.trigger("search:loaded");
		
		// Search if params given
		self.q = q; 
		self.queryPrefix = queryPrefix;
		if (self.q || self.queryPrefix) self.search();
		
		return self;
	};

});
