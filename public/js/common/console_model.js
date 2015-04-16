// Console Model

veda.Module(function (veda) { "use strict";
	
	veda.ConsoleModel = function () { 
	
		var self = riot.observable(this);
		
		// Define Model data setters & getters
		var properties = {script:"", result:"", runat:"", time:""};
		for (var property in properties) {
			(function(property) {
				Object.defineProperty(self, property, {
					get: function() { return properties[property]; },
					set: function(value) { 
						if (properties[property] == value) return; 
						properties[property] = value; 
						self.trigger("property:changed", property, properties[property]);
					}
				});
			})(property);
		}

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
		
		return self;
	
	};
	
});
