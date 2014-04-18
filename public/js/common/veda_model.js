// Veda application Model

"use strict";

var veda = new Veda();

function Veda() {
	var self = $.observable(this);
	
	self._ = guid();
	self._id = "veda";
	self._name = "veda";
	self._path = "/#";
	self._parent = null;
	self._register = function(new_module) { 
		veda[new_module._name] = new_module;
	};

	// Define Model data setters & getters
	var user_uri, ticket, end_time;
	Object.defineProperty(self, "user_uri", {
		get: function() { return user_uri; },
		set: function(value) { if (compare(user_uri, value)) return; user_uri = value; self.trigger("set", "user_uri", value); }
    });
	Object.defineProperty(self, "ticket", {
		get: function() { return ticket; },
		set: function(value) { if (compare(ticket, value)) return; ticket = value; self.trigger("set", "ticket", value); }
    });
	Object.defineProperty(self, "end_time", {
		get: function() { return end_time; },
		set: function(value) { if (compare(end_time, value)) return; end_time = value; self.trigger("set", "end_time", value); }
    });
	if (typeof console != undefined) self.on("set", function(property, value){ console.log("property set:", property, "=", value) });

	// Define Model functions
	self.authenticate = function (username, password) {
		var res = authenticate(username, password);
		self.ticket = res.id;
		self.user_uri = res.user_uri;
		self.end_time = new Date( (res.end_time - 621355968000000000)/10000 );
		if (ticket) self.trigger("authenticate", ticket, end_time);
	};
	self.quit = function() {
		self.ticket = "";
		self.user_uri = "";
		self.end_time = "";
		self.trigger("quit");
	};

	// Define Model event handlers
	self.on("authenticate", function() { 
		veda.RegisterModule( new veda.DocumentModel(self.user_uri), self, "user");
	});

	return self;
};
