// Document Model

function Document() { "use strict";
	var self = $.observable(this);
	self.individual = {};
	self.load = function(uri) {
		self.individual = get_individual(app.ticket, uri);
		self.trigger("loaded");
	};
	self.save = function() {
		put_individual(app.ticket, self.individual);
		self.trigger("saved");
	};
	self.on("loaded", function() { 
		console.log("document loaded: ", self.individual);
	});
	self.on("saved", function() { 
		console.log("document saved: ", self.individual);
	});
};
