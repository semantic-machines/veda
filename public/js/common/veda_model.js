// Veda application Model

"use strict";

function VedaModel(config) {
	var self = riot.observable(this);
	
	// Define Model data setters & getters
	var user_uri, ticket, end_time;
	Object.defineProperty(self, "user_uri", {
		get: function() { return user_uri; },
		set: function(value) { if (compare(user_uri, value)) return; user_uri = value; self.trigger("set", "user_uri", user_uri); }
    });
	Object.defineProperty(self, "ticket", {
		get: function() { return ticket; },
		set: function(value) { if (compare(ticket, value)) return; ticket = value; self.trigger("set", "ticket", ticket); }
    });
	Object.defineProperty(self, "end_time", {
		get: function() { return end_time; },
		set: function(value) { if (compare(end_time, value)) return; end_time = value; self.trigger("set", "end_time", end_time); }
    });
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });

	// Define Model functions
	self.authenticate = function (username, password) {
		var res = authenticate(username, password);
		self.ticket = res.id;
		if (!ticket) return self.trigger("auth:failed", user_uri, ticket, end_time);
		self.user_uri = res.user_uri;
		self.end_time =  Math.floor((res.end_time - 621355968000000000) / 10000 );
		self.trigger("auth:success", user_uri, ticket, end_time);
	};
	self.quit = function() {
		self.ticket = "";
		self.user_uri = "";
		self.end_time = "";
		self.trigger("auth:quit");
	};
	
	// Invoke existing or create new module
	self.load = function (page, params) {
		switch (page) {
			case "console": 
				self.console ? self.trigger("console:loaded", self.console) : RegisterModule(new ConsoleModel(self, params), self, "console"); 
				break
			case "document": 
				self.document ? self.trigger("document:loaded", self.document) : RegisterModule(new DocumentModel(self, params), self, "document"); 
				break
			case "search": 
				self.search ? self.trigger("search:loaded", self.search) : RegisterModule(new SearchModel(self, params), self, "search"); 
				break
			case "user": 
				self.user ? self.trigger("user:loaded", self.user) : RegisterModule(new UserModel(self, params), self, "user"); 
				break
			default: ""; break
		}
	};

	// Define Model event handlers
	self.on("auth:success", function() { 
		RegisterModule( new UserModel(self, [user_uri]), self, "user");
	});

};
