// Veda application Model

"use strict";

function AppModel(config) {
	var self = riot.observable(this);
	
	self.config = config;

	self.started = false;

	// Define Model data setters & getters
	var properties = { user_uri:"", ticket:"", end_time:"" };
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
	
	self.ontology = {};
	self.cache = typeof localStorage != "undefined" ? localStorage : {};
	
	// Invoke existing or create new module
	self.load = function (page, params) {
		switch (page) {
			case "console": 
				self.console ? self.trigger("console:loaded", self.console) : RegisterModule(ConsoleModel.apply(new Object(), [self].concat(params)), self, "console");
				//ConsoleModel.apply(new Object(), [self].concat(params));
				break
			case "document": 
				//self.document ? self.trigger("document:loaded", self.document) : RegisterModule(new DocumentModel(self, ["mondi-data:AdministrativeDocument_1"]), self, "document");
				DocumentModel.apply(new Object(), [self].concat(params));
				break
			case "document2": 
				//self.document ? self.trigger("document:loaded", self.document) : RegisterModule(new DocumentModel(self, ["mondi-data:AdministrativeDocument_1"]), self, "document");
				DocumentModel2.apply(new Object(), [self].concat(params));
				break
			case "search": 
				self.search && ( (self.search._params == params) || params.length == 0 ) ? self.trigger("search:loaded", self.search) && self.trigger("search:complete", self.search) : RegisterModule(SearchModel.apply(new Object(), [self].concat(params)), self, "search", params);
				//new SearchModel(self, params);
				break
			default: ""; break
		}
	};

	// Load ontology after user has been loaded
	self.one("user:loaded", function() {
		var q = "'rdf:type' == 'rdfs:*' || 'rdf:type' == 'rdf:*' || 'rdf:type' == 'owl:*'";
		var q_results = query(self.ticket, q);
		for (var i=0; i<q_results.length; i++) {
			get_individual(self.ticket, q_results[i], function (data) {
				self.cache[data["@"]] = JSON.stringify(data);
				self.ontology[data["@"]] = new IndividualModel(self, data["@"]);
			});
		}
		// App started
		self.started = true;
		self.trigger("app:complete", self.user_uri, self.ticket, self.end_time);
	});
	
	// Load user after successful authentication
	self.on("auth:success", function() { 
		var langs = query(self.ticket, "'rdf:type' == 'veda-ui:Language'");
		self.availableLanguages = langs.reduce ( 
			function (acc, language_uri) {
				var lang = new IndividualModel(self, language_uri);
				acc[lang["rdf:value"][0]] = lang;  
				return acc;
			}, {});
		self.user = new UserModel(self, self.user_uri);
	});
	
	self.on("auth:quit", function() { 
		self.started = false;
	});
	
	return self;
};
