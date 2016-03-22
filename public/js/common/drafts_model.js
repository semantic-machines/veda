// Drafts Model

veda.Module(function (veda) { "use strict";
	
	veda.drafts = (function () {
		
		var self = riot.observable({});
		
		var storage;
		if (typeof localStorage !== "undefined") {
			try {
				storage = JSON.parse(localStorage.drafts);
			} catch (e) {
				delete localStorage.drafts;
				storage = {};
			}
			storage.save = function () {
				localStorage.drafts = JSON.stringify(this);
			};
			storage.clear = function () {
				var self = this;
				Object.keys(self).map(function (key) {
					if (key === "save" || key === "clear") return;
					delete self[key];
				});
				delete localStorage.drafts;
			};
		}
		
		self.get = function (uri) {
			return storage[uri];
		};

		self.set = function (uri, data) {
			storage[uri] = data;
			storage.save();
		};

		self.remove = function (uri) {
			delete storage[uri];
			storage.save();
		};
		
		return self;
		
	})();

});
