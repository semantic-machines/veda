// Module registrator

;(function (veda) { "use strict";

	veda.RegisterModule = function (module, parent, name, params) {

		module._name = name || "";
		module._params = params;
		module._path = !parent._path ? "#/" : parent._path == "#/" ? parent._path + module._name : parent._path + "/" + module._name;
		module._path += params && params.length ? params.reduce(function(p, c) {return p + "/" + c}, "") : "";
		module._parent = parent || null;
		module._register = function(new_module) {
			module[new_module._name] = new_module;
		};
		if (!parent || !name) return module;
		parent._register(module);
		return module;

	};

}(veda));
