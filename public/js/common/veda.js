// Veda application

var app = app || {};

var veda = (function(app) { "use strict";
	var self = $.observable(app);
	self.name = "veda";
	self.api = {
		name: self.name
	};
	self.register = function(module) {
		self.api[module.name] = module;
	}
	self.trigger("ready");
	return self.api;
})(app);