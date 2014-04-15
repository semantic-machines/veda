// Model

"use strict";

function Model(model) {
	model._properties = {};
	function set_setter_getter(property) {
		return function(value) {
			if ( !value && value != "" ) return model._properties[property];
			if ( compare(model._properties[property], value) ) return;
			model._properties[property] = value;
			model.trigger("set", property, value);
		};
	};
	for (var property in model) {
		if (property == "_properties") continue;
		if (typeof model[property] == "function") continue;
		model._properties[property] = model[property]; delete model[property];
		model[property] = set_setter_getter(property);
	};	
	model.on("set", function (property, value) {
		console.log("property was set: " + property + " = " + value);
	});
	return model;
};
