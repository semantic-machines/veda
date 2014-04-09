// Module
function Module(module, app) {
	var self = $.observable(this);
	self.register = function(_module) {
		self[_module.name] = _module;
	}
	for (var i in module) {
		self[i] = module[i];
	}
	if (module.init) self.init();
	self.trigger("ready");
	app.on("ready", app.register(self));
};