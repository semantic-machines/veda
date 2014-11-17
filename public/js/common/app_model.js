// Veda application Model

function AppModel(config) { "use strict";
	
	var self = riot.observable(this);
	
	self.config = config;
	self.user_uri = self.ticket = self.end_time = ""
	self.cache = typeof localStorage != "undefined" ? localStorage : {};
	self.dictionary = {};

	// Define Model functions
	self.login = function (username, password) {
		var res = authenticate(username, password);
		self.ticket = res.id;
		if (!self.ticket) return;
		self.user_uri = res.user_uri;
		self.end_time =  Math.floor((res.end_time - 621355968000000000) / 10000 );
		self.init();
		return {
			ticket: self.ticket, 
			user_uri: self.user_uri, 
			end_time: self.end_time
		}
	};
	
	self.logout = function() {
		self.off("*");
		self = undefined;
		Veda(config);
	};
	
	// Invoke existing or create new module
	self.load = function (page, params) {
		switch (page) {
			case "console": 
				self.console ? self.trigger("console:loaded", self.console) : RegisterModule(ConsoleModel.apply(new Object(), [self].concat(params)), self, "console");
				//ConsoleModel.apply(new Object(), [self].concat(params));
				break
			case "document": 
				//self.document ? self.trigger("document:loaded", self.document) : RegisterModule(new DocumentModel(self, ["mnd-d:AdministrativeDocument_1"]), self, "document");
				DocumentModel.apply(new Object(), [self].concat(params));
				break
			case "search": 
				self.search && ( (self.search._params == params) || params.length == 0 ) ? self.trigger("search:loaded", self.search) && self.trigger("search:complete", self.search) : RegisterModule(SearchModel.apply(new Object(), [self].concat(params)), self, "search", params);
				//new SearchModel(self, params);
				break
			default: ""; break
		}
	};
	
	// Load ontology
	self.init = function () {
		var langs = query(self.ticket, "'rdf:type' == 'v-ui:Language'");
		self.availableLanguages = langs.reduce ( 
			function (acc, language_uri) {
				var lang = new IndividualModel(self, language_uri);
				acc[lang["rdf:value"][0]] = lang;  
				return acc;
			}, {});
		self.user = new UserModel(self, self.user_uri);
		self.ontology = new OntologyModel(self);
		self.trigger("started");
	}
	
	return self;
};

