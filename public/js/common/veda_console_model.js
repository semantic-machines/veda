// Console Model

"use strict";

function ConsoleModel(veda) { 
	var self = riot.observable(this);
	
	// Define Model data setters & getters
	var script, result, runat, time;
	Object.defineProperty(self, "script", {
		get: function() { return script; },
		set: function(value) { if (compare(script, value)) return; script = value; self.trigger("set", "script", value); }
    });
	Object.defineProperty(self, "result", {
		get: function() { return result; },
		set: function(value) { if (compare(result, value)) return; result = value; self.trigger("set", "result", value); }
    });
	Object.defineProperty(self, "runat", {
		get: function() { return runat; },
		set: function(value) { if (compare(runat, value)) return; runat = value; self.trigger("set", "runat", value); }
    });
	Object.defineProperty(self, "time", {
		get: function() { return time; },
		set: function(value) { if (compare(time, value)) return; time = value; self.trigger("set", "time", value); }
    });
	if (typeof console != undefined) self.on("set", function(property, value) { console.log("property set:", property, "=", value) });

	// Define Model functions
	self.run = function() {
		var start = new Date().getTime();
		if (self.runat == "server") {
			execute_script(self.script, function(res) {
				self.result = res[0];
				self.time = new Date().getTime() - start;
			});
		} else {
			var res = eval(self.script);
			self.result = res;
			self.time = new Date().getTime() - start;
		}
	}
	self.reset = function() {
		self.script = "";
		self.result = "";
		self.runat = "";
		self.time = "";
	}
	
	if (veda) veda.trigger("console:loaded", self);
};
