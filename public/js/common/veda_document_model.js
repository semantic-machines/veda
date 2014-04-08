// veda_document Model
;(function (app) { "use strict";
	var self = $.observable({});
	self.name = "document";
	
	self.individual = {};
	
	self.load = function(uri) {
		self.individual = get_individual(app.ticket, uri);
	};
	self.save = function() {
		put_individual(app.ticket, self.individual);
	};
	
	app.on("ready", app.register(self));
})(app);