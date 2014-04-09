// Veda application
var app = app || {};

app = (function(app) { "use strict";
	var self = $.observable(app);
	self.name = "veda";
	self.ticket = authenticate("karpovr", "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3").id;
	self.register = function(module) {
		self[module.name] = module;
	}
	self.trigger("ready");
	return self;
})(app);