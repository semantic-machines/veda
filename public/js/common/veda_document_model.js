// Document Model

"use strict";

function Document(uri) { 
	var self = $.observable(this);
	self.individual = {};
	self.load = function(uri) {
		get_individual(app.ticket, uri, function(data) {
			self.individual = data;
			self.trigger("loaded");
		});
	};
	self.save = function() {
		put_individual(app.ticket, self.individual, function(data) {
			self.trigger("saved");
		});
	};
	self.on("loaded", function() { 
		console.log("document loaded: ", self.individual);
	});
	self.on("saved", function() { 
		console.log("document saved: ", self.individual);
	});
	if (uri) self.load(uri);
};
