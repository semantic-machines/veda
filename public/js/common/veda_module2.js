// Module
function Module(module, parent) {
	var self = $.observable(module);

	self.register = function(new_module) {
		self[new_module.name] = new_module;
	}

	for (var i in module) {
		self[i] = module[i];
	}

	if (module.init) self.init();

	self.trigger("ready");

	if (!parent) return self;
	parent.on("ready", parent.register(self));
};