// Veda application Model

;(function (veda) { "use strict";

	veda.AppModel = function (config) {

		var self = riot.observable(this);
		
		self.config = config;
		self.user_uri = self.ticket = self.end_time = ""
		self.cache = {};
		self.ontology = {};

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
					self.console ? ( 
						self.trigger("console:loaded", self.console)
					) : (
						veda.RegisterModule(veda.ConsoleModel.apply(new Object(), params), self, "console")
					);
					//veda.ConsoleModel.apply(new Object(), params);
					break
				case "document": 
					self.document && ( (self.document._params == params) || params.length == 0 ) ? (
						self.trigger("document:loaded", self.document) 
					) : ( 
						veda.RegisterModule(veda.DocumentModel.apply(new Object(), params), self, "document")
					);
					//veda.DocumentModel.apply(new Object(), params);
					break
				case "search": 
					self.search && ( (self.search._params == params) || params.length == 0 ) ? (
						self.trigger("search:loaded", self.search) && self.trigger("search:complete", self.search) 
					) : ( 
						veda.RegisterModule(veda.SearchModel.apply(new Object(), params), self, "search", params)
					);
					//veda.SearchModel.apply(new Object(), params);
					break
				default: ""; break
			}
		};
		
		// Load ontology
		self.init = function () {
			var langs = query(self.ticket, "'rdf:type' == 'v-ui:Language'");
			self.availableLanguages = langs.reduce ( 
				function (acc, language_uri) {
					var lang = new veda.IndividualModel(language_uri);
					acc[lang["rdf:value"][0]] = lang;  
					return acc;
				}, {});
			self.user = new veda.UserModel(self.user_uri);
			self.ontology = new veda.OntologyModel();
			self.trigger("started");
		}
		
		return self;
	}
	
}(veda));
