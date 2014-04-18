// Module registrator

"use strict";

Veda.prototype.RegisterModule = function RegisterModule(module, parent, name) {
	module._id = guid();
	module._name = name || module._id;
	module._path = parent ? parent._path + "/" + module._name : module._name;
	module._parent = parent || null;
	module._register = function(new_module) {
		module[new_module._name] = new_module;
	};
	module.trigger("ready");
	if (!parent) return module;
	parent.on("ready", parent._register(module));
	return module;
};