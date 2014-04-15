// Console Model

"use strict";

function ConsoleModel() { 
	var self = $.observable(this);
	self.script = self.result = self.runat = self.time = "";
	self.run = function() {
		var start = new Date().getTime();
		if (self.runat() == "server") {
			execute_script(self.script(), function(res) {
				self.result(res[0]);
				self.time(new Date().getTime() - start);
			});
		} else {
			var res = eval(self.script());
			self.result(res);
			self.time(new Date().getTime() - start);
		}
	}
	self.reset = function() {
		self.script("");
		self.result("");
		self.runat("");
		self.time("");
	}
	return self;
};
