// Veda application

var app = app || {};

var veda = (function(app) { "use strict";
	var self = $.observable(app);
	self.name = "veda";
	self.ticket = authenticate("karpovr", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
	self.api = {
		name: self.name
	};
	self.register = function(module) {
		self.api[module.name] = module;
	}
	self.trigger("ready");
	return self.api;
})(app);