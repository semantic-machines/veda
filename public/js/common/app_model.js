// Veda application Model

;(function (veda) { "use strict";

	veda.AppModel = function (config) {

		var self = riot.observable(this);
		
		self.config = config;
		self.user_uri = self.ticket = self.end_time = "";
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
			};
		};
		
		self.logout = function() {
			self.off("*");
			self = undefined;
			veda = new Veda(config);
		};
		
		// Invoke existing or create new module
		self.load = function (page, params) {
			switch (page) {
				case "console": 
					veda.Util.construct(veda.ConsoleModel, params);
					break;
				case "document": 
					veda.Util.construct(veda.DocumentModel, params);
					break;
				case "individual": 
					veda.Util.construct(veda.IndividualModel, params);
					break;
				case "search": 
					if (self.search && ( params == self.search.params || params.length === 0) ) {
						self.trigger("search:loaded", self.search);
						self.trigger("search:complete", self.search);
					} else {
						self.search = veda.SearchModel.apply({}, params);
						self.search.params = params;
					}
					break;
				case "graph": 
					self.trigger("load:graph", params);
					break;
			}
		};
		
		// Load ontology
		self.init = function () {
			self.ontology = new veda.OntologyModel();
			self.user = new veda.UserModel(self.user_uri);
			self.trigger("started");
		};
		
		return self;
	};
	
})(veda);
