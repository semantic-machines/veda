// Module registrator

function Module(module, parent, name) {
	module.name = name;
	module.register = function(new_module) {
		module[new_module.name] = new_module;
	}
	module.trigger("ready");
	if (!parent) return module;
	parent.on("ready", parent.register(module));
};
