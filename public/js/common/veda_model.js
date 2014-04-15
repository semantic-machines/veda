// Model

"use strict";

function Model(model) {
	model.properties = {};
	function set_setter_getter(property) {
		return function(value) {
			if ( !value && value != "" ) return model.properties[property];
			if ( compare(model.properties[property], value) ) return;
			model.properties[property] = value;
			model.trigger("set", property, value);
		};
	};
	for (var property in model) {
		if (property == "properties") continue;
		if (typeof model[property] == "function") continue;
		model.properties[property] = model[property]; delete model[property];
		model[property] = set_setter_getter(property);
	};	
	model.on("set", function (property, value) {
		console.log("property was set: " + property + " = " + value);
	});
	return model;
};
