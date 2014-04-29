// Veda application Model

"use strict";

function AppModel(config) {
	var self = riot.observable(this);
	
	// Define Model data setters & getters
	var properties = { user_uri:"", ticket:"", end_time:"" };
	function define_GS_etters(property) {
		(function(property) {
			Object.defineProperty(self, property, {
				get: function() { return properties[property]; },
				set: function(value) { 
					if (properties[property] == value) return; 
					properties[property] = value; 
					self.trigger("set", property, properties[property]);
				}
   			});
   		})(property);
	};
	for (var property in properties) {
		define_GS_etters(property);
    }
	if (typeof console != "undefined") self.on("set", function(property, value){ console.log("property set:", property, "=", value) });

	// Define Model functions
	self.authenticate = function (username, password) {
		var res = authenticate(username, password);
		self.ticket = res.id;
		if (!self.ticket) return self.trigger("auth:failed", self.user_uri, self.ticket, self.end_time);
		self.user_uri = res.user_uri;
		self.end_time =  Math.floor((res.end_time - 621355968000000000) / 10000 );
		self.trigger("auth:success", self.user_uri, self.ticket, self.end_time);
	};
	self.quit = function() {
		self.ticket = "";
		self.user_uri = "";
		self.end_time = "";
		self.trigger("auth:quit");
	};
	
	self.storage = localStorage ? localStorage : {};
	
	// Invoke existing or create new module
	self.load = function (page, params) {
		switch (page) {
			case "console": 
				//self.console ? self.trigger("console:loaded", self.console) : RegisterModule(new ConsoleModel(self, params), self, "console");
				new ConsoleModel(self, params);
				break
			case "document": 
				//self.document ? self.trigger("document:loaded", self.document) : RegisterModule(new DocumentModel(self, ["mondi-data:AdministrativeDocument_1"]), self, "document");
				new DocumentModel(self, params);
				break
			case "search": 
				//self.search ? self.trigger("search:loaded", self.search) : RegisterModule(new SearchModel(self, params), self, "search"); 
				new SearchModel(self, params);
				break
			case "user": 
				//self.user ? self.trigger("user:loaded", self.user) : RegisterModule(new UserModel(self, params), self, "user"); 
				new UserModel(self, params);
				break
			default: ""; break
		}
	};

	// App load complete after user & ontology has been loaded
	self.on("user:loaded", function() {
		// Load ontology
		var q = "'rdf:type' == 'rdfs:Class' || 'rdf:type' == 'owl:Class' || 'rdf:type' == 'rdf:Property' || 'rdf:type' == 'owl:DatatypeProperty' || 'rdf:type' == 'owl:ObjectProperty'";
		var q_results = query(self.ticket, q);
		for (var i=0; i<q_results.length; i++) {
			var ontology_individual = get_individual(self.ticket, q_results[i], function (data) {
				self.storage[data["@"]] = JSON.stringify(data);
			});
		}
		self.trigger("app:complete", self.user_uri, self.ticket, self.end_time);
	});
	
	// Define Model event handlers
	self.on("auth:success", function() { 
		RegisterModule(new UserModel(self, [self.user_uri]), self, "user");
	});
	
	
};
